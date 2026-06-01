{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.World.Growth.Solver.PrimordialField.Engine
  ( runWorldSolver
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Melusine.World.Growth.Internal.Channels
  ( Channels(Channels, chCols, chData)
  )
import Melusine.World.Growth.Solver.Common.ActiveRegion
  ( ActiveRegion(..)
  , ActiveRegionCause(..)
  , SolverEpoch(..)
  , activePairRegionFromDirtyFacesAt
  )
import Melusine.World.Growth.Solver.Common.Substrate
  ( SolverSubstrate(..)
  )
import Moonlight.Analysis.Mesh.Scalar
  ( ScalarLevelOp
  , MGLevelOp
  , MGArena
  , buildMGOperator
  , buildScalarLevelOp
  , newMGArena
  , scalarLevelRelativeDrift
  )
import Moonlight.Analysis.Mesh.Multigrid
  ( MGHierarchy
  )
import Melusine.World.Growth.Internal.Graph
  ( ConductanceState
      ( ConductanceState
      , csFaceActivity
      , csFaceDownhill
      , csFaceOutInv
      , csFaceOutSum
      , csFaceRoughness
      , csPairDiffusionWeights
      , csPairFluxMem
      , csPairPuissanceDiff
      , csPairWeights
      )
  )
import Melusine.World.Growth.Solver.PrimordialField.Calibration.Laws
  ( VolumeMBOParams(..)
  , advancePhasesInPlace
  , advanceClaimsInPlace
  , advancePressuresInto
  , buildPressureDiffusionMatrix
  , computeClaimPotentialsInto
  , fusedEdgeSweepInto
  )
import Melusine.World.Growth.Solver.PrimordialField.Pressure.Mass
  ( targetMassFromSoftMass
  , updateClaimMassActive
  )
import Melusine.World.Growth.Solver.PrimordialField.Core.Types
  ( PressureStepMethod(..)
  , SolverTuning(..)
  , WorldContext(..)
  , WorldState(..)
  , pressureScheduleCanChangeAfter
  , pressureScheduleInitialStep
  , pressureScheduleNextStep
  )
import Melusine.World.Growth.Solver.PrimordialField.Pressure.Block
  ( PressureBlock
  , MPressureBlock(..)
  , PressureInput(..)
  , freezePressure
  , freezePressureClone
  , newPressureMutable
  , pressureCopy
  , pressureDotM
  , pressureMaxAbsDeltaM
  , pressureMixInto
  , pressureRead8M
  , pressureResidualInto
  , pressureRows
  , thawPressure
  )
import Melusine.World.Growth.Solver.PrimordialField.Pressure.CoupledStep
  ( CoupledArena
  , CoupledField
  , CoupledPlanReuse(..)
  , coupledArenaDeltaWithDirty
  , materializeCoupledArena
  , newCoupledArena
  , pressureCoupledField
  , readCoupledFace8
  , retargetCoupledArena
  , seedCoupledArena
  , snapshotCoupledArena
  , solveCoupledFieldActive
  )
import Melusine.World.Growth.Solver.PrimordialField.Report
  ( MultigridRebuildCause(..)
  , MultigridReuse(..)
  , MultigridReuseWitness(..)
  , PrimordialImplicitStepReport(..)
  , PrimordialRetargetReport(..)
  , PrimordialSolverReport
  , emptyPrimordialReportAcc
  , finalizePrimordialReportAcc
  , recordImplicitStep
  )
import Melusine.World.Growth.Solver.PrimordialField.Pressure.Projection
  ( densePressureReader
  , modalPressureReader
  )
import Melusine.World.Growth.Solver.PrimordialField.Core.Schedule
  ( AdaptParams(..)
  , StageParams(..)
  )
import Melusine.World.Growth.Solver.PrimordialField.Pressure.Sources
  ( SourceModel(..)
  , SparseClaims(..)
  , refreshSparseClaims
  )
import Melusine.World.Growth.Internal.Stats
  ( computeGaussianUnaryInto
  )
import Melusine.World.Numeric (clamp)
import Melusine.World.Growth.Core.Types
  ( AxisId(AxisId)
  , etherAxis
  , lithicAxis
  , luminosityAxis
  )

type PhaseArena :: Type -> Type
data PhaseArena s = PhaseArena
  { phaBlend    :: !(MPressureBlock s)
  , phaWeights  :: !(VUM.MVector s Float)
  , phaUnary    :: !(VUM.MVector s Float)
  , phaDelta    :: !(VUM.MVector s Float)
  , phaSoftMass :: !(VUM.MVector s Float)
  , phaMass     :: !(VUM.MVector s Float)
  , phaBias     :: !(VUM.MVector s Float)
  }

phaseColsFrom :: PhaseArena s -> Int
phaseColsFrom = VUM.length . phaMass
{-# INLINE phaseColsFrom #-}

type SharedArena :: Type -> Type
data SharedArena s = SharedArena
  { shaSourceBlend   :: !(MPressureBlock s)
  , shaPhase         :: !(PhaseArena s)
  , shaClaimWeights  :: !(VUM.MVector s Float)
  , shaClaimPot      :: !(VUM.MVector s Float)
  , shaClaimDelta    :: !(VUM.MVector s Float)
  , shaPairWeights   :: !(VUM.MVector s Float)
  , shaPairDiffusionWeights :: !(VUM.MVector s Float)
  , shaPairFluxMem   :: !(VUM.MVector s Float)
  , shaPairPuissanceDiff :: !(VUM.MVector s Float)
  , shaFaceOutSum    :: !(VUM.MVector s Float)
  , shaFaceOutInv    :: !(VUM.MVector s Float)
  , shaFaceActivity  :: !(VUM.MVector s Float)
  , shaFaceDownhill  :: !(VUM.MVector s Float)
  , shaFaceRoughness :: !(VUM.MVector s Float)
  , shaClaimMass     :: !(VUM.MVector s Float)
  , shaDirtySeen     :: !(VUM.MVector s Word8)
  , shaDirtyBuf      :: !(VUM.MVector s Int)
  , shaScratch0      :: !(VUM.MVector s Float)
  , shaScratch1      :: !(VUM.MVector s Float)
  , shaScratch2      :: !(VUM.MVector s Float)
  }

type ExplicitArena :: Type -> Type
data ExplicitArena s = ExplicitArena
  { epaPressCur   :: !(MPressureBlock s)
  , epaPressNext  :: !(MPressureBlock s)
  , epaPressureLap :: !(MPressureBlock s)
  , epaPrevOut    :: !(MPressureBlock s)
  , epaPrevRes    :: !(MPressureBlock s)
  , epaResTmp     :: !(MPressureBlock s)
  , epaDfTmp      :: !(MPressureBlock s)
  }

type ImplicitArena :: Type -> Type
data ImplicitArena s = ImplicitArena
  { ipaFineOp    :: !ScalarLevelOp
  , ipaMgAnchor  :: !ScalarLevelOp
  , ipaMgOp      :: !MGLevelOp
  , ipaCoupled   :: !(CoupledArena s)
  , ipaMGArena   :: !(MGArena s)
  }

thawPhaseArena :: Int -> WorldState -> ST s (PhaseArena s)
thawPhaseArena !rows !st0 = do
  let !phaseCols = chCols (wsPhases st0)

  blend <- newPressureMutable rows 0.0
  weights <- VU.thaw (chData (wsPhases st0))
  unary <- VU.thaw (chData (wsPhaseUnary st0))
  delta <- VUM.replicate (rows * phaseCols) (0.0 :: Float)
  softMass <- VU.thaw (wsUnarySoftMass st0)
  mass <- VU.thaw (wsPhaseMass st0)
  bias <- VU.thaw (wsPhaseBias st0)

  pure
    PhaseArena
      { phaBlend = blend
      , phaWeights = weights
      , phaUnary = unary
      , phaDelta = delta
      , phaSoftMass = softMass
      , phaMass = mass
      , phaBias = bias
      }

thawSharedArena :: WorldState -> ST s (SharedArena s)
thawSharedArena !st0 = do
  let !rows = pressureRows (wsPressures st0)
      !k = scK (wsClaims st0)
      !phaseCols0 = chCols (wsPhases st0)
      !maxCols = max 1 (max k phaseCols0)

  sourceBlend <- newPressureMutable rows 0.0
  phase <- thawPhaseArena rows st0

  claimWeights <- VU.thaw (scWeights (wsClaims st0))
  claimPot <- VU.thaw (wsClaimPotentials st0)
  claimDelta <- VUM.replicate (rows * k) (0.0 :: Float)

  pairWeights <- VU.thaw (csPairWeights (wsConductance st0))
  pairDiffusionWeights <- VU.thaw (csPairDiffusionWeights (wsConductance st0))
  pairFluxMem <- VU.thaw (csPairFluxMem (wsConductance st0))
  pairPuissanceDiff <- VU.thaw (csPairPuissanceDiff (wsConductance st0))
  faceOutSum <- VU.thaw (csFaceOutSum (wsConductance st0))
  faceOutInv <- VU.thaw (csFaceOutInv (wsConductance st0))
  faceActivity <- VU.thaw (csFaceActivity (wsConductance st0))
  faceDownhill <- VU.thaw (csFaceDownhill (wsConductance st0))
  faceRoughness <- VU.thaw (csFaceRoughness (wsConductance st0))

  claimMass <- VU.thaw (wsClaimMass st0)

  dirtySeen <- VUM.replicate rows 0
  dirtyBuf <- VUM.unsafeNew rows

  scratch0 <- VUM.replicate maxCols (0.0 :: Float)
  scratch1 <- VUM.replicate maxCols (0.0 :: Float)
  scratch2 <- VUM.replicate maxCols (0.0 :: Float)

  pure
    SharedArena
      { shaSourceBlend = sourceBlend
      , shaPhase = phase
      , shaClaimWeights = claimWeights
      , shaClaimPot = claimPot
      , shaClaimDelta = claimDelta
      , shaPairWeights = pairWeights
      , shaPairDiffusionWeights = pairDiffusionWeights
      , shaPairFluxMem = pairFluxMem
      , shaPairPuissanceDiff = pairPuissanceDiff
      , shaFaceOutSum = faceOutSum
      , shaFaceOutInv = faceOutInv
      , shaFaceActivity = faceActivity
      , shaFaceDownhill = faceDownhill
      , shaFaceRoughness = faceRoughness
      , shaClaimMass = claimMass
      , shaDirtySeen = dirtySeen
      , shaDirtyBuf = dirtyBuf
      , shaScratch0 = scratch0
      , shaScratch1 = scratch1
      , shaScratch2 = scratch2
      }

thawExplicitArena :: WorldState -> ST s (ExplicitArena s)
thawExplicitArena !st0 = do
  let !rows = pressureRows (wsPressures st0)
  pressCur <- thawPressure (wsPressures st0)
  pressNext <- newPressureMutable rows 0.0
  pressureLap <- newPressureMutable rows 0.0
  prevOut <- maybe (newPressureMutable rows 0.0) thawPressure (wsPrevPressureOut st0)
  prevRes <- maybe (newPressureMutable rows 0.0) thawPressure (wsPrevResidual st0)
  resTmp <- newPressureMutable rows 0.0
  dfTmp <- newPressureMutable rows 0.0
  pure
    ExplicitArena
      { epaPressCur = pressCur
      , epaPressNext = pressNext
      , epaPressureLap = pressureLap
      , epaPrevOut = prevOut
      , epaPrevRes = prevRes
      , epaResTmp = resTmp
      , epaDfTmp = dfTmp
      }

freezePhaseState
  :: Int
  -> PhaseArena s
  -> ST s (Channels, Channels, VU.Vector Float, VU.Vector Float, VU.Vector Float)
freezePhaseState !rows !phase = do
  let !phaseCols = phaseColsFrom phase

  phaseWts <- VU.unsafeFreeze (phaWeights phase)
  phaseUn <- VU.unsafeFreeze (phaUnary phase)
  softMass <- VU.unsafeFreeze (phaSoftMass phase)
  phaseMass <- VU.unsafeFreeze (phaMass phase)
  phaseBias <- VU.unsafeFreeze (phaBias phase)

  pure
    ( Channels rows phaseCols phaseWts
    , Channels rows phaseCols phaseUn
    , softMass
    , phaseMass
    , phaseBias
    )

freezeWorldState
  :: WorldState
  -> SharedArena s
  -> SparseClaims
  -> AdaptParams
  -> VU.Vector Int
  -> PressureBlock
  -> Maybe PressureBlock
  -> Maybe PressureBlock
  -> Int
  -> ST s WorldState
freezeWorldState !st0 !shared !layout !adapt !dirtyFaces !press !prevOut !prevRes !iterDone = do
  let !rows = scRows layout
      !phase = shaPhase shared

  claimWts <- VU.unsafeFreeze (shaClaimWeights shared)
  claimPot <- VU.unsafeFreeze (shaClaimPot shared)
  (phases, unary, usm, bm, bb) <- freezePhaseState rows phase

  pairWts <- VU.unsafeFreeze (shaPairWeights shared)
  pairDiffusionWts <- VU.unsafeFreeze (shaPairDiffusionWeights shared)
  pairFlx <- VU.unsafeFreeze (shaPairFluxMem shared)
  pairPdf <- VU.unsafeFreeze (shaPairPuissanceDiff shared)
  fos <- VU.unsafeFreeze (shaFaceOutSum shared)
  foi <- VU.unsafeFreeze (shaFaceOutInv shared)
  fa <- VU.unsafeFreeze (shaFaceActivity shared)
  fd <- VU.unsafeFreeze (shaFaceDownhill shared)
  fr <- VU.unsafeFreeze (shaFaceRoughness shared)

  cm <- VU.unsafeFreeze (shaClaimMass shared)

  let !claims = layout { scWeights = claimWts }
      !cond = ConductanceState pairWts pairDiffusionWts pairFlx pairPdf fos foi fa fd fr

  pure
    st0
      { wsIteration = iterDone
      , wsPressures = press
      , wsClaims = claims
      , wsPhases = phases
      , wsConductance = cond
      , wsClaimPotentials = claimPot
      , wsPhaseUnary = unary
      , wsAdapt = adapt
      , wsDirtyFaces = dirtyFaces
      , wsClaimMass = cm
      , wsUnarySoftMass = usm
      , wsPhaseMass = bm
      , wsPhaseBias = bb
      , wsPrevPressureOut = prevOut
      , wsPrevResidual = prevRes
      }

clearDirtySeen :: SharedArena s -> VU.Vector Int -> ST s ()
clearDirtySeen !shared !prevDirty = do
  let !count = VU.length prevDirty
      go !ix
        | ix == count = pure ()
        | otherwise = do
            VUM.unsafeWrite (shaDirtySeen shared) (VU.unsafeIndex prevDirty ix) 0
            go (ix + 1)
  go 0

markDirtyFace :: SharedArena s -> STRef s Int -> Int -> ST s ()
markDirtyFace !shared !countRef !i = do
  seen <- VUM.unsafeRead (shaDirtySeen shared) i
  when (seen == 0) $ do
    n <- readSTRef countRef
    VUM.unsafeWrite (shaDirtySeen shared) i 1
    VUM.unsafeWrite (shaDirtyBuf shared) n i
    writeSTRef countRef (n + 1)

freezeDirtyFaces :: SharedArena s -> STRef s Int -> ST s (VU.Vector Int)
freezeDirtyFaces !shared !countRef = do
  n <- readSTRef countRef
  VU.generateM n (VUM.unsafeRead (shaDirtyBuf shared))

writeVec :: VU.Unbox a => VUM.MVector s a -> VU.Vector a -> ST s ()
writeVec !dst !src = do
  let !n = min (VUM.length dst) (VU.length src)
      go !ix
        | ix == n = pure ()
        | otherwise = VUM.unsafeWrite dst ix (VU.unsafeIndex src ix) >> go (ix + 1)
  go 0

mkPressureInputs :: WorldContext -> SharedArena s -> [PressureInput s]
mkPressureInputs !ctx !shared =
  let !phase = shaPhase shared
  in
    [ PressureInput
        { piPull = wcSourcePull ctx
        , piBlend = shaSourceBlend shared
        }
    , PressureInput
        { piPull = vpPressurePull (wcPhaseLaw ctx)
        , piBlend = phaBlend phase
        }
    ]
{-# INLINE mkPressureInputs #-}

copyPressureRowDelta
  :: Int
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> Int
  -> ST s Float
copyPressureRowDelta !rows !dst !src !i = do
  let !ix0 = i
      !ix1 = rows + i
      !ix2 = ix1 + rows
      !ix3 = ix2 + rows
      !ix4 = ix3 + rows
      !ix5 = ix4 + rows
      !ix6 = ix5 + rows
      !ix7 = ix6 + rows

  o0 <- VUM.unsafeRead dst ix0; n0 <- VUM.unsafeRead src ix0
  o1 <- VUM.unsafeRead dst ix1; n1 <- VUM.unsafeRead src ix1
  o2 <- VUM.unsafeRead dst ix2; n2 <- VUM.unsafeRead src ix2
  o3 <- VUM.unsafeRead dst ix3; n3 <- VUM.unsafeRead src ix3
  o4 <- VUM.unsafeRead dst ix4; n4 <- VUM.unsafeRead src ix4
  o5 <- VUM.unsafeRead dst ix5; n5 <- VUM.unsafeRead src ix5
  o6 <- VUM.unsafeRead dst ix6; n6 <- VUM.unsafeRead src ix6
  o7 <- VUM.unsafeRead dst ix7; n7 <- VUM.unsafeRead src ix7

  VUM.unsafeWrite dst ix0 n0
  VUM.unsafeWrite dst ix1 n1
  VUM.unsafeWrite dst ix2 n2
  VUM.unsafeWrite dst ix3 n3
  VUM.unsafeWrite dst ix4 n4
  VUM.unsafeWrite dst ix5 n5
  VUM.unsafeWrite dst ix6 n6
  VUM.unsafeWrite dst ix7 n7

  let !d0 = abs (n0 - o0)
      !d1 = abs (n1 - o1)
      !d2 = abs (n2 - o2)
      !d3 = abs (n3 - o3)
      !d4 = abs (n4 - o4)
      !d5 = abs (n5 - o5)
      !d6 = abs (n6 - o6)
      !d7 = abs (n7 - o7)

  pure $
    max d0
      (max d1
        (max d2
          (max d3
            (max d4
              (max d5
                (max d6 d7))))))

{-# INLINE copyPressureRowDelta #-}

copyPressureAllWithDirty
  :: SharedArena s -> STRef s Int -> Double
  -> MPressureBlock s -> MPressureBlock s -> ST s Double
copyPressureAllWithDirty !shared !dirtyRef !eps !(MPressureBlock rows dst) !(MPressureBlock _ src) = do
  let !epsF = realToFrac eps :: Float
      faceLoop !i !best
        | i == rows = pure (realToFrac best)
        | otherwise = do
            d <- copyPressureRowDelta rows dst src i
            when (d > epsF) (markDirtyFace shared dirtyRef i)
            faceLoop (i + 1) (max best d)
  faceLoop 0 0.0

copyPressureFacesWithDirty
  :: SharedArena s -> STRef s Int -> Double
  -> VU.Vector Int -> MPressureBlock s -> MPressureBlock s -> ST s Double
copyPressureFacesWithDirty !shared !dirtyRef !eps !faces !(MPressureBlock rows dst) !(MPressureBlock _ src) = do
  let !epsF = realToFrac eps :: Float
      !count = VU.length faces
      faceLoop !ix !best
        | ix == count = pure (realToFrac best)
        | otherwise = do
            let !i = VU.unsafeIndex faces ix
            d <- copyPressureRowDelta rows dst src i
            when (d > epsF) (markDirtyFace shared dirtyRef i)
            faceLoop (ix + 1) (max best d)
  faceLoop 0 0.0

applyAnderson1InPlace
  :: SolverTuning
  -> Int
  -> SharedArena s
  -> ExplicitArena s
  -> STRef s Int
  -> Double
  -> Bool
  -> ST s (Double, Bool)
applyAnderson1InPlace !tuning !iter !shared !explicit !dirtyRef !eps !havePrev = do
  pressureResidualInto (epaResTmp explicit) (epaPressNext explicit) (epaPressCur explicit)
  if iter < stAndersonStart tuning || not havePrev
    then do
      delta <- copyPressureAllWithDirty shared dirtyRef eps (epaPressCur explicit) (epaPressNext explicit)
      pressureCopy (epaPrevOut explicit) (epaPressNext explicit)
      pressureCopy (epaPrevRes explicit) (epaResTmp explicit)
      pure (delta, True)
    else do
      pressureResidualInto (epaDfTmp explicit) (epaResTmp explicit) (epaPrevRes explicit)
      den <- pressureDotM (epaDfTmp explicit) (epaDfTmp explicit)
      num <- pressureDotM (epaResTmp explicit) (epaDfTmp explicit)
      let !denD = realToFrac den :: Double
          !numD = realToFrac num :: Double
          !alpha0 = if denD <= 1.0e-12 then 0.0 else (- numD / denD)
          !alpha = clamp (- stAndersonMaxMix tuning) (stAndersonMaxMix tuning) alpha0
      candDelta <- pressureMaxAbsDeltaM (epaPressCur explicit) (epaPressNext explicit)
      let !alphaF = realToFrac alpha :: Float
      pressureMixInto (1.0 - alphaF) (epaDfTmp explicit) alphaF (epaPressNext explicit) (epaPrevOut explicit)
      mixDelta <- pressureMaxAbsDeltaM (epaPressCur explicit) (epaDfTmp explicit)
      delta <-
        if mixDelta <= 2.0 * candDelta
          then copyPressureAllWithDirty shared dirtyRef eps (epaPressCur explicit) (epaDfTmp explicit)
          else copyPressureAllWithDirty shared dirtyRef eps (epaPressCur explicit) (epaPressNext explicit)
      pressureCopy (epaPrevOut explicit) (epaPressNext explicit)
      pressureCopy (epaPrevRes explicit) (epaResTmp explicit)
      pure (delta, True)

updatePhaseBiasInPlace
  :: VolumeMBOParams -> Int -> VU.Vector Float
  -> VUM.MVector s Float -> VUM.MVector s Float -> ST s ()
updatePhaseBiasInPlace !params !rows !targetMass !phaseMass !phaseBias = do
  let !cols = min (VU.length targetMass) (VUM.length phaseBias)
      go !b
        | b == cols = pure ()
        | otherwise = do
            oldB <- VUM.unsafeRead phaseBias b
            massB <- VUM.unsafeRead phaseMass b
            let !nextB = oldB + realToFrac (vpBiasGain params) * (VU.unsafeIndex targetMass b - massB / max 1.0 (fromIntegral rows))
            VUM.unsafeWrite phaseBias b nextB
            go (b + 1)
  go 0

runWorldSolver :: WorldContext -> WorldState -> (WorldState, PrimordialSolverReport)
runWorldSolver !ctx !st0 = runST $ do
  shared <- thawSharedArena st0
  explicit <- thawExplicitArena st0
  let !substrate = wcSubstrate ctx
      !graph = ssGraph substrate
      !hierarchy = ssMgHierarchy substrate
  mgAr <- newMGArena hierarchy

  let !steps = wcIterations ctx
      !checkpoint1 = steps `quot` 3
      !checkpoint2 = (2 * steps) `quot` 3
      !rows = pressureRows (wsPressures st0)
      !sourceCount = smCount (wcSourceModel ctx)
      !denseSmall = sourceCount > 0 && sourceCount < 8 && scK (wsClaims st0) == sourceCount
      !tuning = wcTuning ctx
      !pressureSchedule = stPressureSchedule tuning
      !initialPressureStep = pressureScheduleInitialStep pressureSchedule
      pressureStepAfter !method !iterDone !delta =
        pressureScheduleNextStep pressureSchedule method iterDone delta
      scheduleHasLaterSwitch !method !iterDone =
        pressureScheduleCanChangeAfter pressureSchedule method iterDone
      !fullRegion = ssFullPairRegion substrate
      !fullFaces = arHaloFaces fullRegion
      !fullPairs = arSupport fullRegion
      !slowEvery = 3 :: Int

      diffusionCoeffs !adapt !stage =
        VU.imap
          (\a !base ->
              let !ax = AxisId (fromIntegral a)
                  !extra =
                    if ax == etherAxis || ax == lithicAxis || ax == luminosityAxis
                      then spExoticMul stage
                      else 1.0
              in base * spDiffusionMul stage * extra)
          (apPressureDiffusion adapt)

      rebuildCoupledField !adapt !stage =
        let !diffCoeffs = diffusionCoeffs adapt stage
            !mixMat = buildPressureDiffusionMatrix (wcPressureLaw ctx) diffCoeffs
        in pressureCoupledField
             (wcPressureLaw ctx)
             (mkPressureInputs ctx shared)
             mixMat
             (shaFaceActivity shared)
             (shaFaceDownhill shared)
             (shaFaceRoughness shared)

      rebuildScalarLevelOp =
        buildScalarLevelOp graph (shaPairDiffusionWeights shared)

      newImplicitArenaFromExplicit !adapt !iterationIndex = do
        fineOp' <- rebuildScalarLevelOp
        let !field' = rebuildCoupledField adapt (wcStageAt ctx iterationIndex steps)
            !mgOp' = buildMGOperator hierarchy fineOp'
        coupled' <- newCoupledArena rows field'
        seedCoupledArena (pressureRead8M (epaPressCur explicit)) coupled' rows fullFaces
        pure (ImplicitArena fineOp' fineOp' mgOp' coupled' mgAr)

      seedExplicitFromImplicit !implicitArena = do
        press <- materializeCoupledArena (ipaCoupled implicitArena) rows fullFaces
        mutablePress <- thawPressure press
        pressureCopy (epaPressCur explicit) mutablePress

      implicitMGRebuildRelTol :: Float
      implicitMGRebuildRelTol = 0.10

      freezeExplicitState !layout !adapt !dirtyFaces !havePrev !iterDone = do
        press <- freezePressure (epaPressCur explicit)
        prevOut <-
          if havePrev
            then Just <$> freezePressure (epaPrevOut explicit)
            else pure Nothing
        prevRes <-
          if havePrev
            then Just <$> freezePressure (epaPrevRes explicit)
            else pure Nothing
        freezeWorldState st0 shared layout adapt dirtyFaces press prevOut prevRes iterDone

      freezeImplicitState !layout !adapt !dirtyFaces !iterDone !implicitArena = do
        press <- materializeCoupledArena (ipaCoupled implicitArena) rows fullFaces
        freezeWorldState st0 shared layout adapt dirtyFaces press Nothing Nothing iterDone

      runCoupledStep
        !pressureReader
        !maybePressureLap
        !layout1
        !activeFaces
        !activePairs
        !adapt
        !stage
        !diffCoeffs
        !cEps
        !bEps
        !dirtyRef = do
        let !phase = shaPhase shared
            !phaseCols = phaseColsFrom phase

        computeClaimPotentialsInto
          (wcClaimPotentialLaw ctx)
          (spSourceEpochs stage)
          (wcSourceModel ctx)
          pressureReader
          layout1
          (shaClaimPot shared)
          activeFaces

        computeGaussianUnaryInto
          (wcPhaseModel ctx)
          pressureReader
          (phaUnary phase)
          (phaSoftMass phase)
          activeFaces
          (shaScratch0 shared)
          (shaScratch1 shared)

        softMass <-
          VU.generateM
            (VUM.length (phaSoftMass phase))
            (VUM.unsafeRead (phaSoftMass phase))
        let !targetMass = targetMassFromSoftMass rows softMass

        fusedEdgeSweepInto
          (wcConductanceLaw ctx)
          (wcPressureLaw ctx)
          (wcTransportLaw ctx)
          denseSmall
          (apContrastSharpness adapt * spContrastMul stage)
          (spConductanceMix stage)
          (spConductanceBeta stage)
          (spTransportGain stage)
          diffCoeffs
          (graph)
          pressureReader
          layout1
          (shaClaimWeights shared)
          (shaClaimPot shared)
          phaseCols
          (phaWeights phase)
          (shaPairWeights shared)
          (shaPairDiffusionWeights shared)
          (shaPairFluxMem shared)
          (shaPairPuissanceDiff shared)
          (shaFaceOutSum shared)
          (shaFaceOutInv shared)
          (shaFaceActivity shared)
          (shaFaceDownhill shared)
          (shaFaceRoughness shared)
          (shaClaimDelta shared)
          (phaDelta phase)
          maybePressureLap
          activeFaces
          activePairs

        advanceClaimsInPlace
          (wcTransportLaw ctx)
          (spSourceEpochs stage)
          (wcSourceModel ctx)
          layout1
          (shaFaceOutInv shared)
          (shaClaimWeights shared)
          (shaClaimPot shared)
          (shaClaimDelta shared)
          (shaClaimMass shared)
          (shaSourceBlend shared)
          activeFaces
          (shaScratch0 shared)
          cEps
          (markDirtyFace shared dirtyRef)

        advancePhasesInPlace
          (wcPhaseLaw ctx)
          (spPhaseForce stage)
          (spThresholdBlend stage)
          (shaFaceOutInv shared)
          rows
          phaseCols
          (phaWeights phase)
          (phaUnary phase)
          (phaDelta phase)
          (phaBias phase)
          (phaMass phase)
          (wcPhaseActuation ctx)
          (phaBlend phase)
          activeFaces
          (shaScratch0 shared)
          (shaScratch1 shared)
          (shaScratch2 shared)
          bEps
          (markDirtyFace shared dirtyRef)

        updatePhaseBiasInPlace
          (wcPhaseLaw ctx)
          rows
          targetMass
          (phaMass phase)
          (phaBias phase)

      refreshLayout !layout !activeFaces !refreshNow
        | not refreshNow = pure layout
        | otherwise = do
            snapW <- VU.freeze (shaClaimWeights shared)
            oldMass <- VU.freeze (shaClaimMass shared)
            let !layoutView = layout { scWeights = snapW }
                !layout' =
                  refreshSparseClaims
                    (graph)
                    (ssFaceEmbedding substrate)
                    (wcSourceModel ctx)
                    activeFaces
                    layoutView
                !newMass = updateClaimMassActive sourceCount layoutView layout' activeFaces oldMass
            writeVec (shaClaimWeights shared) (scWeights layout')
            writeVec (shaClaimMass shared) newMass
            pure layout'

      goExplicit !reportAcc !t !layout !dirtyFaces !adapt !havePrev
        | t >= steps = do
            finalState <- freezeExplicitState layout adapt dirtyFaces havePrev t
            pure (finalState, reportAcc)
        | otherwise = do
            let !epoch = SolverEpoch t
                !fullRegionAtEpoch = fullRegion { arEpoch = epoch }
                !region
                  | t < stFrontierWarmup tuning = fullRegionAtEpoch
                  | VU.length dirtyFaces >= rows = fullRegionAtEpoch
                  | otherwise =
                      activePairRegionFromDirtyFacesAt
                        epoch
                        ActiveRegionDirtyFaces
                        (stFrontierFullSweepFrac tuning)
                        fullRegion
                        graph
                        dirtyFaces
                !activeFaces = arHaloFaces region
                !activePairs = arSupport region
                !fullSweep = arIsFull region || VU.length activeFaces >= rows

            if not fullSweep && t >= stFrontierWarmup tuning && VU.null activeFaces
              then do
                finalState <- freezeExplicitState layout adapt dirtyFaces havePrev t
                pure (finalState, reportAcc)
              else do
                clearDirtySeen shared dirtyFaces
                dirtyRef <- newSTRef 0

                let !iterDone = t + 1
                    !stage = wcStageAt ctx t steps
                    !mul
                      | t < stFrontierWarmup tuning = 1.0
                      | t < stFrontierWarmup tuning + 4 = 1.5
                      | otherwise = stDirtyLateMul tuning
                    !pEps = mul * stDirtyPressureEps tuning
                    !cEps = mul * stDirtyClaimEps tuning
                    !bEps = mul * stDirtyPhaseEps tuning
                    !refreshNow =
                      not denseSmall
                        && (t == 0 || iterDone == checkpoint1 || iterDone == checkpoint2)
                    !diffCoeffs = diffusionCoeffs adapt stage

                layout1 <- refreshLayout layout activeFaces refreshNow
                runCoupledStep
                  (densePressureReader (epaPressCur explicit))
                  (Just (epaPressureLap explicit))
                  layout1
                  activeFaces
                  activePairs
                  adapt
                  stage
                  diffCoeffs
                  cEps
                  bEps
                  dirtyRef

                advancePressuresInto
                  (wcPressureLaw ctx)
                  (mkPressureInputs ctx shared)
                  (graph)
                  (shaFaceOutInv shared)
                  (shaFaceActivity shared)
                  (shaFaceDownhill shared)
                  (shaFaceRoughness shared)
                  (epaPressCur explicit)
                  (epaPressureLap explicit)
                  (epaPressNext explicit)
                  activeFaces

                (delta, havePrev1) <-
                  if fullSweep
                    then applyAnderson1InPlace tuning t shared explicit dirtyRef pEps havePrev
                    else do
                      d <- copyPressureFacesWithDirty shared dirtyRef pEps activeFaces (epaPressCur explicit) (epaPressNext explicit)
                      pure (d, False)

                dirtyFaces' <- freezeDirtyFaces shared dirtyRef

                adapt1 <-
                  if iterDone == checkpoint1 || iterDone == checkpoint2
                    then do
                      pressSnap <- freezePressureClone (epaPressCur explicit)
                      pure $ wcAdaptFeedback ctx pressSnap adapt
                    else pure adapt

                case pressureStepAfter ExplicitAndersonStep iterDone (realToFrac delta) of
                  ImplicitPCGStep -> do
                    implicitArena <- newImplicitArenaFromExplicit adapt1 iterDone
                    goImplicit reportAcc True iterDone layout1 dirtyFaces' adapt1 implicitArena
                  ExplicitAndersonStep
                    | not (scheduleHasLaterSwitch ExplicitAndersonStep iterDone) && iterDone >= 6 && delta < stDirtyPressureEps tuning -> do
                        finalState <- freezeExplicitState layout1 adapt1 dirtyFaces' havePrev1 iterDone
                        pure (finalState, reportAcc)
                    | otherwise ->
                        goExplicit reportAcc iterDone layout1 dirtyFaces' adapt1 havePrev1

      goImplicit !reportAcc !forceHeavy !t !layout !dirtyFaces !adapt !implicitArena
        | t >= steps = do
            finalState <- freezeImplicitState layout adapt dirtyFaces t implicitArena
            pure (finalState, reportAcc)
        | otherwise = do
            clearDirtySeen shared dirtyFaces
            dirtyRef <- newSTRef 0

            let !iterDone = t + 1
                !stage = wcStageAt ctx t steps
                !diffCoeffs = diffusionCoeffs adapt stage
                !pEps = stDirtyPressureEps tuning
                !cEps = stDirtyClaimEps tuning
                !bEps = stDirtyPhaseEps tuning
                !heavyNow = forceHeavy || t == 0 || t `mod` slowEvery == 0
                !refreshNow =
                  heavyNow
                    && not denseSmall
                    && (t == 0 || iterDone == checkpoint1 || iterDone == checkpoint2)

            layout1 <- refreshLayout layout fullFaces refreshNow

            (!implicitArena1, !maybeRetargetReport) <-
              if heavyNow
                then do
                  runCoupledStep
                    (modalPressureReader (readCoupledFace8 (ipaCoupled implicitArena) rows))
                    Nothing
                    layout1
                    fullFaces
                    fullPairs
                    adapt
                    stage
                    diffCoeffs
                    cEps
                    bEps
                    dirtyRef

                  fineOp' <- rebuildScalarLevelOp

                  let !field' =
                        rebuildCoupledField adapt stage

                  (!implicitArenaRetargeted, !mgReuse, !coupledReuse) <-
                    retargetImplicitArena
                      hierarchy
                      implicitMGRebuildRelTol
                      fineOp'
                      field'
                      implicitArena
                      rows
                      fullFaces

                  pure
                    ( implicitArenaRetargeted,
                      Just
                        PrimordialRetargetReport
                          { prrMultigridReuse = mgReuse,
                            prrCoupledPlanReuse = coupledReuse
                          }
                    )
                else
                  pure (implicitArena, Nothing)

            snapshotCoupledArena (ipaCoupled implicitArena1)

            let !field1 = rebuildCoupledField adapt stage

            coupledSolveStats <-
              solveCoupledFieldActive
                (stKrylov tuning)
                (ipaFineOp implicitArena1)
                (ipaMgOp implicitArena1)
                (ipaMGArena implicitArena1)
                field1
                (readCoupledFace8 (ipaCoupled implicitArena1) rows)
                (ipaCoupled implicitArena1)

            let !stepReport =
                  PrimordialImplicitStepReport
                    { pisrIteration = iterDone,
                      pisrHeavyStep = heavyNow,
                      pisrRefreshClaims = refreshNow,
                      pisrRetarget = maybeRetargetReport,
                      pisrCoupledSolve = coupledSolveStats
                    }
                !reportAcc1 =
                  recordImplicitStep stepReport reportAcc

            delta <- coupledArenaDeltaWithDirty
              (ipaCoupled implicitArena1)
              rows
              fullFaces
              (realToFrac pEps)
              (markDirtyFace shared dirtyRef)
            dirtyFaces' <- freezeDirtyFaces shared dirtyRef

            adapt1 <-
              if iterDone == checkpoint1 || iterDone == checkpoint2
                then do
                  pressSnap <- materializeCoupledArena (ipaCoupled implicitArena1) rows fullFaces
                  pure $ wcAdaptFeedback ctx pressSnap adapt
                else pure adapt

            case pressureStepAfter ImplicitPCGStep iterDone (realToFrac delta) of
              ExplicitAndersonStep -> do
                seedExplicitFromImplicit implicitArena1
                goExplicit reportAcc1 iterDone layout1 dirtyFaces' adapt1 False
              ImplicitPCGStep
                | not (scheduleHasLaterSwitch ImplicitPCGStep iterDone) && iterDone >= 6 && realToFrac delta < stDirtyPressureEps tuning -> do
                    finalState <- freezeImplicitState layout1 adapt1 dirtyFaces' iterDone implicitArena1
                    pure (finalState, reportAcc1)
                | otherwise ->
                    goImplicit reportAcc1 False iterDone layout1 dirtyFaces' adapt1 implicitArena1

  (!finalState, !reportAccFinal) <-
    case initialPressureStep of
      ExplicitAndersonStep ->
        goExplicit
          emptyPrimordialReportAcc
          0
          (wsClaims st0)
          (wsDirtyFaces st0)
          (wsAdapt st0)
          (isJust (wsPrevPressureOut st0) && isJust (wsPrevResidual st0))
      ImplicitPCGStep -> do
        implicitArena <- newImplicitArenaFromExplicit (wsAdapt st0) 0
        goImplicit
          emptyPrimordialReportAcc
          True
          0
          (wsClaims st0)
          (wsDirtyFaces st0)
          (wsAdapt st0)
          implicitArena

  pure (finalState, finalizePrimordialReportAcc reportAccFinal)

retargetImplicitArena ::
  MGHierarchy ->
  Float ->
  ScalarLevelOp ->
  CoupledField s ->
  ImplicitArena s ->
  Int ->
  VU.Vector Int ->
  ST s (ImplicitArena s, MultigridReuse, CoupledPlanReuse)
retargetImplicitArena
  !hierarchy
  !tolerance
  !fineOp'
  !field'
  !arena
  !rows
  !fullFaces = do
    let !relativeDrift =
          scalarLevelRelativeDrift (ipaMgAnchor arena) fineOp'
        !witness =
          MultigridReuseWitness
            { mrwRelativeDrift = relativeDrift,
              mrwTolerance = tolerance
            }
        !rebuildMG =
          relativeDrift > tolerance
        !mgReuse =
          if rebuildMG
            then MultigridRebuilt (MultigridDriftTooLarge witness)
            else MultigridReused witness
        !mgAnchor' =
          if rebuildMG
            then fineOp'
            else ipaMgAnchor arena
        !mgOp' =
          if rebuildMG
            then buildMGOperator hierarchy fineOp'
            else ipaMgOp arena

    (!coupled', !coupledReuse) <-
      retargetCoupledArena
        field'
        (ipaCoupled arena)
        rows
        fullFaces

    pure
      ( arena
          { ipaFineOp = fineOp',
            ipaMgAnchor = mgAnchor',
            ipaMgOp = mgOp',
            ipaCoupled = coupled'
          },
        mgReuse,
        coupledReuse
      )
