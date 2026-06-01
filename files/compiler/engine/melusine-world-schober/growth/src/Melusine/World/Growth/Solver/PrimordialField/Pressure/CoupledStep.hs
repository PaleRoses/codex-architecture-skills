{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.World.Growth.Solver.PrimordialField.Pressure.CoupledStep
  ( Linear8(..)
  , CoupledExpr(..)
  , CoupledField(..)
  , CoupledArena
  , CoupledPlanReuse(..)
  , CoupledModeStats(..)
  , CoupledSolveStats(..)
  , pressureCoupledField
  , newCoupledArena
  , seedCoupledArena
  , retargetCoupledArena
  , snapshotCoupledArena
  , readCoupledFace8
  , materializeCoupledArena
  , coupledArenaDeltaWithDirty
  , coupledArenaPullLeak
  , solveCoupledFieldActive
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Kind (Type)
import Data.List (sortBy)
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Moonlight.Analysis.Mesh.Graph (Graph(grFaces))
import Moonlight.Analysis.Mesh.Krylov (KrylovReport(..), KrylovTuning)
import Moonlight.Analysis.Mesh.Scalar
  ( MGLevelOp
  , MGArena
  , ScalarLevelOp(..)
  , solveScalarMGPCG
  )

import Melusine.World.Growth.Solver.PrimordialField.Core.LawTypes
  ( PressureLaw
      ( plClipHi
      , plClipLo
      , plDt
      , plNodeForce
      , plReactionGain
      , plReactionMat
      )
  )
import Melusine.World.Growth.Solver.PrimordialField.Core.Terrain
  ( terrainSignalFromLocal
  )
import Melusine.World.Growth.Solver.Common.Util (MFloatVec)
import Melusine.World.Growth.Solver.PrimordialField.Pressure.Block
  ( Pressure8(..)
  , PressureInput(..)
  , PressureBlock
  , add8
  , applyMatrix8
  , diagMul8
  , freezePressure
  , maxAbs8
  , newPressureMutable
  , pressureInputsAt
  , pressureInputsPull
  , pressureWrite8
  , scale8
  , sub8
  , tanh8
  )

type Linear8 :: Type
data Linear8
  = Id8
  | Diag8 !(VU.Vector Double)
  | Mat8 !(VU.Vector Double)

applyLinear8 :: Linear8 -> Pressure8 -> Pressure8
applyLinear8 Id8 = id
applyLinear8 (Diag8 d) = diagMul8 d
applyLinear8 (Mat8 m) = applyMatrix8 m
{-# INLINE applyLinear8 #-}

type CoupledExpr :: Type -> Type
data CoupledExpr s
  = CEZero
  | CEState
  | CEInput !(Int -> ST s Pressure8)
  | CEAdd !(CoupledExpr s) !(CoupledExpr s)
  | CELinear !Linear8 !(CoupledExpr s)
  | CETanh !(CoupledExpr s)
  | CELocal !(Int -> Pressure8 -> ST s Pressure8)

zero8 :: Pressure8
zero8 = Pressure8 0 0 0 0 0 0 0 0
{-# INLINE zero8 #-}

evalCoupledExprAt :: CoupledExpr s -> Pressure8 -> Int -> ST s Pressure8
evalCoupledExprAt !expr !p0 !i =
  case expr of
    CEZero -> pure zero8
    CEState -> pure p0
    CEInput readInp -> readInp i
    CEAdd a b -> do
      xa <- evalCoupledExprAt a p0 i
      xb <- evalCoupledExprAt b p0 i
      pure (add8 xa xb)
    CELinear op e -> applyLinear8 op <$> evalCoupledExprAt e p0 i
    CETanh e -> tanh8 <$> evalCoupledExprAt e p0 i
    CELocal f -> f i p0
{-# INLINE evalCoupledExprAt #-}

type CoupledField :: Type -> Type
data CoupledField s = CoupledField
  { cfDt           :: !Double
  , cfImplicitPull :: !(VU.Vector Double)
  , cfDiffusionMat :: !(VU.Vector Double)
  , cfKnown        :: !(CoupledExpr s)
  , cfClipLo       :: !Float
  , cfClipHi       :: !Float
  , cfRank         :: !Int
  }

pressureRetainedModeCount :: Int
pressureRetainedModeCount = 6
{-# INLINE pressureRetainedModeCount #-}

pressureCoupledField
  :: PressureLaw
  -> [PressureInput s]
  -> VU.Vector Double
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> CoupledField s
pressureCoupledField
  !law !inputs !mixMat
  !faceActivity !faceDownhill !faceRoughness =
    CoupledField
      { cfDt = plDt law
      , cfImplicitPull = pressureInputsPull inputs
      , cfDiffusionMat = mixMat
      , cfKnown = CEAdd (CEInput (pressureInputsAt inputs)) (CEAdd reactionExpr nodeExpr)
      , cfClipLo = realToFrac (plClipLo law)
      , cfClipHi = realToFrac (plClipHi law)
      , cfRank = pressureRetainedModeCount
      }
  where
    reactionExpr =
      CELinear
        (Diag8 (plReactionGain law))
        (CETanh (CELinear (Mat8 (plReactionMat law)) CEState))

    nodeExpr =
      CELocal $ \ !i !p0 -> do
        pathI <- VUM.unsafeRead faceActivity i
        downI <- VUM.unsafeRead faceDownhill i
        roughI <- VUM.unsafeRead faceRoughness i
        let !witness =
              terrainSignalFromLocal
                (realToFrac pathI)
                (realToFrac downI)
                (realToFrac roughI)
                p0
        pure (plNodeForce law (realToFrac pathI) (realToFrac downI) (realToFrac roughI) witness p0)

type CoupledPlan :: Type
data CoupledPlan = CoupledPlan
  { cpRank     :: !Int
  , cpEigVals  :: !(VU.Vector Float)
  , cpToModal  :: !(VU.Vector Float)
  , cpPulls    :: !(VU.Vector Float)
  , cpPullLeak :: !Float
  , cpClipLo   :: !Float
  , cpClipHi   :: !Float
  }

type CoupledArena :: Type -> Type
data CoupledArena s = CoupledArena
  { caPlan      :: !CoupledPlan
  , caXBufs     :: !(VUM.MVector s Float)
  , caPrevXBufs :: !(VUM.MVector s Float)
  , caBBufs     :: !(VUM.MVector s Float)
  , caWorkR     :: !(MFloatVec s)
  , caWorkP     :: !(MFloatVec s)
  , caWorkZ     :: !(MFloatVec s)
  , caWorkAp    :: !(MFloatVec s)
  }

type CoupledPlanReuse :: Type
data CoupledPlanReuse
  = CoupledPlanReusedSameRank !Int
  | CoupledArenaReallocatedForRankChange !Int !Int
  deriving stock (Eq, Show)

type CoupledModeStats :: Type
data CoupledModeStats = CoupledModeStats
  { cmsModeIndex     :: !Int
  , cmsEigenvalue    :: !Float
  , cmsPull          :: !Float
  , cmsRhsNorm       :: !Float
  , cmsResidual      :: !Float
  , cmsIterations     :: !Int
  , cmsSolutionDelta :: !Float
  }
  deriving stock (Eq, Show)

type CoupledSolveStats :: Type
data CoupledSolveStats = CoupledSolveStats
  { cssModes            :: !(VB.Vector CoupledModeStats)
  , cssMaxResidual      :: !Float
  , cssMaxSolutionDelta :: !Float
  , cssMaxRhsNorm       :: !Float
  , cssMaxIterations    :: !Int
  }
  deriving stock (Eq, Show)

emptyCoupledSolveStats :: CoupledSolveStats
emptyCoupledSolveStats =
  CoupledSolveStats
    { cssModes = VB.empty
    , cssMaxResidual = 0.0
    , cssMaxSolutionDelta = 0.0
    , cssMaxRhsNorm = 0.0
    , cssMaxIterations = 0
    }

coupledArenaPullLeak :: CoupledArena s -> Float
coupledArenaPullLeak = cpPullLeak . caPlan
{-# INLINE coupledArenaPullLeak #-}

symmetrize8 :: VU.Vector Double -> VU.Vector Double
symmetrize8 !m =
  VU.generate 64 $ \ix ->
    let (!r, !c) = ix `quotRem` 8
    in 0.5 * (VU.unsafeIndex m (r * 8 + c) + VU.unsafeIndex m (c * 8 + r))

makeSPD8 :: VU.Vector Double -> VU.Vector Double
makeSPD8 !m =
  let !sym = symmetrize8 m
      rowOffAbs !r !c !acc
        | c == 8 = acc
        | c == r = rowOffAbs r (c + 1) acc
        | otherwise = rowOffAbs r (c + 1) (acc + abs (VU.unsafeIndex sym (r * 8 + c)))
      diagFloor !r =
        let !oldD = VU.unsafeIndex sym (r * 8 + r)
            !need = rowOffAbs r 0 0.0 + 1.0e-6
        in max oldD need
  in VU.generate 64 $ \ix ->
       let (!r, !c) = ix `quotRem` 8
       in if r == c then diagFloor r else VU.unsafeIndex sym ix

jacobiEigen8 :: VU.Vector Double -> (VU.Vector Double, VU.Vector Double)
jacobiEigen8 !a0 = runST $ do
  a <- VU.thaw a0
  v <- VUM.replicate 64 0.0
  let readA !r !c = VUM.unsafeRead a (r * 8 + c)
      writeA !r !c !x = VUM.unsafeWrite a (r * 8 + c) x
      readV !r !c = VUM.unsafeRead v (r * 8 + c)
      writeV !r !c !x = VUM.unsafeWrite v (r * 8 + c) x
      initId !r !c
        | r == 8 = pure ()
        | c == 8 = initId (r + 1) 0
        | otherwise = do
            writeV r c (if r == c then 1.0 else 0.0)
            initId r (c + 1)
      findMax = do
        let go !r !c !br !bc !best
              | r == 8 = pure (br, bc, best)
              | c == 8 = go (r + 1) (r + 2) br bc best
              | otherwise = do
                  x <- abs <$> readA r c
                  if x > best then go r (c + 1) r c x else go r (c + 1) br bc best
        go 0 1 0 1 0.0
      rotate !p !q = do
        app <- readA p p; aqq <- readA q q; apq <- readA p q
        when (abs apq > 1.0e-14) $ do
          let !tau = (aqq - app) / (2.0 * apq)
              !t = if tau >= 0.0 then 1.0 / (tau + sqrt (1.0 + tau * tau))
                   else -1.0 / ((-tau) + sqrt (1.0 + tau * tau))
              !c = 1.0 / sqrt (1.0 + t * t)
              !s = t * c
          mapM_ (\k -> when (k /= p && k /= q) $ do
                    akp <- readA k p; akq <- readA k q
                    let !akp' = c * akp - s * akq
                        !akq' = s * akp + c * akq
                    writeA k p akp'; writeA p k akp'
                    writeA k q akq'; writeA q k akq') [0..7]
          writeA p p (app - t * apq)
          writeA q q (aqq + t * apq)
          writeA p q 0.0
          writeA q p 0.0
          mapM_ (\k -> do
                    vkp <- readV k p
                    vkq <- readV k q
                    writeV k p (c * vkp - s * vkq)
                    writeV k q (s * vkp + c * vkq)) [0..7]
      sweep (!k :: Int)
        | k == 0 = pure ()
        | otherwise = do
            (p, q, best) <- findMax
            if best <= 1.0e-10 then pure () else rotate p q >> sweep (k - 1)
  initId 0 0
  sweep 64
  eigVals <- VU.generateM 8 $ \i -> readA i i
  eigVecs <- VU.unsafeFreeze v
  pure (eigVals, eigVecs)

modalPullLeak :: Int -> VU.Vector Float -> VU.Vector Double -> Float
modalPullLeak !rank !u !pull =
  let entry !m !n =
        let go !a !acc
              | a == 8 = acc
              | otherwise =
                  let !um = VU.unsafeIndex u (m * 8 + a)
                      !un = VU.unsafeIndex u (n * 8 + a)
                      !pa = realToFrac (VU.unsafeIndex pull a) :: Float
                  in go (a + 1) (acc + um * pa * un)
        in go 0 0.0
      outer !m !n !best
        | m == rank = best
        | n == rank = outer (m + 1) 0 best
        | m == n = outer m (n + 1) best
        | otherwise = outer m (n + 1) (max best (abs (entry m n)))
  in outer 0 0 0.0

compileCoupledPlan :: CoupledField s -> CoupledPlan
compileCoupledPlan !field =
  let !spd = makeSPD8 (cfDiffusionMat field)
      (!eigVals0, !eigVecs0) = jacobiEigen8 spd
      !order = sortBy (\i j -> compare (VU.unsafeIndex eigVals0 j) (VU.unsafeIndex eigVals0 i)) [0..7]
      !rank = max 1 (min 8 (cfRank field))
      !keep = VU.fromList (take rank order)
      !eigVals = VU.map (realToFrac . max 1.0e-6 . VU.unsafeIndex eigVals0) keep
      !toModal = VU.generate (rank * 8) $ \ix ->
        let (!m, !a) = ix `quotRem` 8
            !col = VU.unsafeIndex keep m
        in realToFrac (VU.unsafeIndex eigVecs0 (a * 8 + col)) :: Float
      !pull = cfImplicitPull field
      !pulls = VU.generate rank $ \modeIx ->
        let go !a !acc
              | a == 8 = acc
              | otherwise =
                  let !u = VU.unsafeIndex toModal (modeIx * 8 + a)
                      !d = realToFrac (VU.unsafeIndex pull a) :: Float
                  in go (a + 1) (acc + u * u * d)
        in go 0 0.0
      !pullLeak = modalPullLeak rank toModal pull
  in CoupledPlan
      { cpRank = rank
      , cpEigVals = eigVals
      , cpToModal = toModal
      , cpPulls = pulls
      , cpPullLeak = pullLeak
      , cpClipLo = cfClipLo field
      , cpClipHi = cfClipHi field
      }

newFloatVec :: Int -> ST s (MFloatVec s)
newFloatVec !n = VUM.replicate n (0.0 :: Float)

newCoupledArena :: Int -> CoupledField s -> ST s (CoupledArena s)
newCoupledArena !rows !field = do
  let !plan = compileCoupledPlan field
      !rank = cpRank plan
  xBufs <- VUM.replicate (rank * rows) (0.0 :: Float)
  prevXBufs <- VUM.replicate (rank * rows) (0.0 :: Float)
  bBufs <- VUM.replicate (rank * rows) (0.0 :: Float)
  workR <- newFloatVec rows
  workP <- newFloatVec rows
  workZ <- newFloatVec rows
  workAp <- newFloatVec rows
  pure (CoupledArena plan xBufs prevXBufs bBufs workR workP workZ workAp)

projectPressureF :: CoupledPlan -> Int -> Pressure8 -> Float
projectPressureF !plan !modeIx (Pressure8 x0 x1 x2 x3 x4 x5 x6 x7) =
  let !u = cpToModal plan
      !b = modeIx * 8
      c !a = VU.unsafeIndex u (b + a)
  in c 0 * realToFrac x0 + c 1 * realToFrac x1 + c 2 * realToFrac x2 + c 3 * realToFrac x3
   + c 4 * realToFrac x4 + c 5 * realToFrac x5 + c 6 * realToFrac x6 + c 7 * realToFrac x7
{-# INLINE projectPressureF #-}

reconstructPressure8 :: CoupledPlan -> VUM.MVector s Float -> Int -> Int -> ST s Pressure8
reconstructPressure8 !plan !xBufs !rows !i =
  let !rank = cpRank plan
      !u = cpToModal plan
      go !m !a0 !a1 !a2 !a3 !a4 !a5 !a6 !a7
        | m == rank =
            pure (Pressure8 (realToFrac a0) (realToFrac a1) (realToFrac a2) (realToFrac a3)
                            (realToFrac a4) (realToFrac a5) (realToFrac a6) (realToFrac a7))
        | otherwise = do
            xm <- VUM.unsafeRead xBufs (m * rows + i)
            let !xd = realToFrac xm :: Double
                !b = m * 8
                coeff !k = realToFrac (VU.unsafeIndex u (b + k)) :: Double
            go (m + 1)
              (a0 + xd * coeff 0) (a1 + xd * coeff 1) (a2 + xd * coeff 2) (a3 + xd * coeff 3)
              (a4 + xd * coeff 4) (a5 + xd * coeff 5) (a6 + xd * coeff 6) (a7 + xd * coeff 7)
  in go 0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0

clipWith :: Float -> Float -> Pressure8 -> Pressure8
clipWith !lo !hi (Pressure8 a0 a1 a2 a3 a4 a5 a6 a7) =
  let clamp !x = max lo (min hi x)
  in Pressure8
      (clamp a0) (clamp a1) (clamp a2) (clamp a3)
      (clamp a4) (clamp a5) (clamp a6) (clamp a7)
{-# INLINE clipWith #-}

projectWith :: (Int -> ST s Pressure8) -> CoupledPlan -> CoupledArena s -> Int -> VU.Vector Int -> ST s ()
projectWith !readFace !plan !arena !rows !activeFaces = do
  let !rank = cpRank plan
      !xBufs = caXBufs arena
      !n = VU.length activeFaces
      faceLoop !ix
        | ix == n = pure ()
        | otherwise = do
            let !i = VU.unsafeIndex activeFaces ix
            p0 <- readFace i
            let modeLoop !m
                  | m == rank = pure ()
                  | otherwise = do
                      VUM.unsafeWrite xBufs (m * rows + i) (projectPressureF plan m p0)
                      modeLoop (m + 1)
            modeLoop 0
            faceLoop (ix + 1)
  faceLoop 0

seedCoupledArena :: (Int -> ST s Pressure8) -> CoupledArena s -> Int -> VU.Vector Int -> ST s ()
seedCoupledArena !readFace !arena !rows !activeFaces =
  projectWith readFace (caPlan arena) arena rows activeFaces

snapshotCoupledArena :: CoupledArena s -> ST s ()
snapshotCoupledArena !arena =
  VUM.copy (caPrevXBufs arena) (caXBufs arena)
{-# INLINE snapshotCoupledArena #-}

retargetCoupledArena :: CoupledField s -> CoupledArena s -> Int -> VU.Vector Int -> ST s (CoupledArena s, CoupledPlanReuse)
retargetCoupledArena !field !oldArena !rows !activeFaces = do
  let !oldPlan = caPlan oldArena
      !newPlan = compileCoupledPlan field
      !sameRank = cpRank oldPlan == cpRank newPlan
  if sameRank
    then do
      snapshotCoupledArena oldArena
      let readOld !i = reconstructPressure8 oldPlan (caPrevXBufs oldArena) rows i
      projectWith readOld newPlan oldArena rows activeFaces
      pure (oldArena { caPlan = newPlan }, CoupledPlanReusedSameRank (cpRank newPlan))
    else do
      newArena <- newCoupledArena rows field
      let readOld !i = reconstructPressure8 oldPlan (caXBufs oldArena) rows i
      projectWith readOld (caPlan newArena) newArena rows activeFaces
      pure (newArena, CoupledArenaReallocatedForRankChange (cpRank oldPlan) (cpRank newPlan))

readCoupledFace8 :: CoupledArena s -> Int -> Int -> ST s Pressure8
readCoupledFace8 !arena !rows !i =
  reconstructPressure8 (caPlan arena) (caXBufs arena) rows i
{-# INLINE readCoupledFace8 #-}

materializeCoupledArena :: CoupledArena s -> Int -> VU.Vector Int -> ST s PressureBlock
materializeCoupledArena !arena !rows !activeFaces = do
  out <- newPressureMutable rows 0.0
  let !plan = caPlan arena
      !lo = cpClipLo plan
      !hi = cpClipHi plan
      !n = VU.length activeFaces
      faceLoop !ix
        | ix == n = pure ()
        | otherwise = do
            let !i = VU.unsafeIndex activeFaces ix
            p <- clipWith lo hi <$> reconstructPressure8 plan (caXBufs arena) rows i
            pressureWrite8 out i p
            faceLoop (ix + 1)
  faceLoop 0
  freezePressure out

coupledArenaDeltaWithDirty
  :: CoupledArena s
  -> Int
  -> VU.Vector Int
  -> Float
  -> (Int -> ST s ())
  -> ST s Float
coupledArenaDeltaWithDirty !arena !rows !activeFaces !eps !markDirty = do
  let !plan = caPlan arena
      !lo = cpClipLo plan
      !hi = cpClipHi plan
      !n = VU.length activeFaces
      faceLoop !ix !best
        | ix == n = pure best
        | otherwise = do
            let !i = VU.unsafeIndex activeFaces ix
            oldP <- clipWith lo hi <$> reconstructPressure8 plan (caPrevXBufs arena) rows i
            newP <- clipWith lo hi <$> reconstructPressure8 plan (caXBufs arena) rows i
            let !d = maxAbs8 (sub8 newP oldP)
            when (d > eps) (markDirty i)
            faceLoop (ix + 1) (max best d)
  faceLoop 0 0.0

solveCoupledFieldActive
  :: KrylovTuning
  -> ScalarLevelOp
  -> MGLevelOp
  -> MGArena s
  -> CoupledField s
  -> (Int -> ST s Pressure8)
  -> CoupledArena s
  -> ST s CoupledSolveStats
solveCoupledFieldActive
  !tuning !fineOp !mgOp !mga !field !readState !arena
  | rows <= 0 = pure emptyCoupledSolveStats
  | rank <= 0 = pure emptyCoupledSolveStats
  | otherwise = do
      buildAll 0
      modeStats <- VB.generateM rank solveMode
      pure
        CoupledSolveStats
          { cssModes = modeStats
          , cssMaxResidual = foldModeStats cmsResidual modeStats
          , cssMaxSolutionDelta = foldModeStats cmsSolutionDelta modeStats
          , cssMaxRhsNorm = foldModeStats cmsRhsNorm modeStats
          , cssMaxIterations = foldModeStatsInt cmsIterations modeStats
          }
  where
    !rows = grFaces (sloGraph fineOp)
    !plan = caPlan arena
    !dtF = realToFrac (cfDt field) :: Float
    !rank = cpRank plan
    !xBufs = caXBufs arena
    !prevXBufs = caPrevXBufs arena
    !bBufs = caBBufs arena
    !mass = sloMass fineOp

    buildRhsFace !i = do
      p0 <- readState i
      known <- evalCoupledExprAt (cfKnown field) p0 i
      let !mi = VU.unsafeIndex mass i
          !rhs8 = scale8 mi (add8 p0 (scale8 dtF known))
          modeLoop !m
            | m == rank = pure ()
            | otherwise = do
                VUM.unsafeWrite bBufs (m * rows + i) (projectPressureF plan m rhs8)
                modeLoop (m + 1)
      modeLoop 0

    buildAll !i
      | i == rows = pure ()
      | otherwise = buildRhsFace i >> buildAll (i + 1)

    solveMode !m = do
      let !base = m * rows
          !xSlice = VUM.unsafeSlice base rows xBufs
          !prevSlice = VUM.unsafeSlice base rows prevXBufs
          !bSlice = VUM.unsafeSlice base rows bBufs
          !pull = VU.unsafeIndex (cpPulls plan) m
          !diff = VU.unsafeIndex (cpEigVals plan) m

      rhsNorm <- mutableL2Norm rows bSlice
      report <-
        solveScalarMGPCG
          tuning
          fineOp
          mgOp
          mga
          dtF
          pull
          diff
          xSlice
          bSlice
          (caWorkR arena)
          (caWorkP arena)
          (caWorkZ arena)
          (caWorkAp arena)
      solutionDelta <- mutableMaxAbsDelta rows xSlice prevSlice

      pure
        CoupledModeStats
          { cmsModeIndex = m
          , cmsEigenvalue = diff
          , cmsPull = pull
          , cmsRhsNorm = rhsNorm
          , cmsResidual = krFinalResidual report
          , cmsIterations = krIterations report
          , cmsSolutionDelta = solutionDelta
          }

mutableL2Norm :: Int -> VUM.MVector s Float -> ST s Float
mutableL2Norm !n !vec =
  if n <= 0
    then pure 0.0
    else do
      ss <- go 0 0.0
      pure (sqrt (ss / fromIntegral n))
  where
    go !i !acc
      | i == n = pure acc
      | otherwise = do
          x <- VUM.unsafeRead vec i
          go (i + 1) (acc + x * x)

mutableMaxAbsDelta :: Int -> VUM.MVector s Float -> VUM.MVector s Float -> ST s Float
mutableMaxAbsDelta !n !left !right =
  go 0 0.0
  where
    go !i !best
      | i == n = pure best
      | otherwise = do
          l <- VUM.unsafeRead left i
          r <- VUM.unsafeRead right i
          go (i + 1) (max best (abs (l - r)))

foldModeStats :: (CoupledModeStats -> Float) -> VB.Vector CoupledModeStats -> Float
foldModeStats !project =
  VB.foldl' (\ !best !stats -> max best (project stats)) 0.0

foldModeStatsInt :: (CoupledModeStats -> Int) -> VB.Vector CoupledModeStats -> Int
foldModeStatsInt !project =
  VB.foldl' (\ !best !stats -> max best (project stats)) 0
