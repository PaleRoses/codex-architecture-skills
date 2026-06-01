{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.World.Growth.Solver.Solstice.Hydrology.Arena
  ( HydrologyArenaError(..)
  , HydrologyMode(..)
  , MorphogenicFrontier(..)
  , HydrologyState(..)
  , HydrologyArena(..)
  , newHydrologyArena
  , writeHydrologyCarry
  , readHydrologyCarry
  , cacheHydrologyRelief
  , buildMorphogenicFrontier
  , fullMorphogenicFrontier
  , advanceHydrologyArena
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Kind (Type)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Melusine.World.Growth.Internal.Channels
  ( ScalarField
  )
import Melusine.World.Growth.Solver.Solstice.Drainage
  ( DrainageError(..)
  , DrainageInput(..)
  , DrainageResult(..)
  , OutletMask
  , drainFixedTopography
  )
import Melusine.World.Growth.Solver.Solstice.Laws.Drainage
  ( DrainageLaw(..)
  )
import Melusine.World.Growth.Solver.Common.MeshOps (isOutletAt)
import Melusine.World.Growth.Solver.Common.Relief
  ( ReliefBlock(..)
  , MReliefBlock(..)
  , thawRelief
  , reliefData
  , freezeReliefClone
  , reliefReadM
  , reliefWrite
  )
import Melusine.World.Growth.Solver.Common.Util (MIntVec, MWord8Vec, positivePow)
import Melusine.World.Growth.Solver.Solstice.Laws.Relief
  ( MorphogenicLaw(..)
  )
import Melusine.World.Growth.Solver.Common.ActiveRegion
  ( ActiveRegion(..)
  , ActiveRegionCause(..)
  , DirectedEdgeRegion
  , SolverEpoch
  , emptyActiveRegionAt
  , fullDirectedEdgeRegionAt
  )
import Melusine.World.Growth.Solver.Common.TerrainMesh
  ( TerrainMesh(..)
  , faceDistance
  , slotIx
  )
import Melusine.World.Growth.Solver.Solstice.WaterSurface
  ( BasinSurface(..)
  , WaterSurfaceError(..)
  , WaterSurfaceResult(..)
  , advanceBasinSurface
  , projectBasinSurface
  , buildHydraulicElevation
  , buildHydraulicOutletMask
  , buildWaterSurfaceFromBasin
  )
import Moonlight.Analysis.Mesh.Graph
  ( Graph(..)
  , edgeRange
  )

type HydrologyArenaError :: Type
data HydrologyArenaError
  = HydrologyArenaDrainage !DrainageError
  | HydrologyArenaWater !WaterSurfaceError
  | HydrologyArenaInvalid !String
  deriving stock (Eq, Show)

type HydrologyMode :: Type
data HydrologyMode
  = HydrologyAdvance
  | HydrologyProject
  deriving stock (Eq, Show)

type MorphogenicFrontier :: Type
data MorphogenicFrontier = MorphogenicFrontier
  { mfRegion      :: !DirectedEdgeRegion
  , mfDirtyBasins :: !(VU.Vector Int)
  }

type HydrologyState :: Type
data HydrologyState = HydrologyState
  { hsRows                :: !Int
  , hsBasin               :: !BasinSurface
  , hsWater               :: !WaterSurfaceResult
  , hsHydraulicElevation  :: !ScalarField
  , hsHydraulicOutletMask :: !OutletMask
  , hsDrainage            :: !DrainageResult
  , hsDirtyFaces          :: !(VU.Vector Int)
  , hsDirtyBasins         :: !(VU.Vector Int)
  , hsActiveFaces         :: !(VU.Vector Int)
  , hsActivePairs         :: !(VU.Vector Int)
  }

type HydrologyArena :: Type -> Type
data HydrologyArena s = HydrologyArena
  { haLakeCarry  :: !(MReliefBlock s)
  , haPrevRelief :: !(MReliefBlock s)
  , haDirtySeen  :: !(MWord8Vec s)
  , haDirtyBuf   :: !(MIntVec s)
  , haBasinSeen  :: !(MWord8Vec s)
  , haBasinBuf   :: !(MIntVec s)
  , haActiveSeen :: !(MWord8Vec s)
  , haActiveBuf  :: !(MIntVec s)
  , haPairSeen   :: !(MWord8Vec s)
  , haPairBuf    :: !(MIntVec s)
  }

newHydrologyArena
  :: ScalarField
  -> ScalarField
  -> TerrainMesh
  -> ST s (HydrologyArena s)
newHydrologyArena !relief0 !lakeCarry0 !mesh = do
  let !rows = tmRows mesh
      !edgeCount = VU.length (grNbrs (tmGraph mesh))
  carry <- thawRelief (ReliefBlock rows lakeCarry0)
  prevRelief <- thawRelief (ReliefBlock rows relief0)
  dirtySeen <- VUM.replicate rows (0 :: Word8)
  dirtyBuf <- VUM.unsafeNew rows
  basinSeen <- VUM.replicate rows (0 :: Word8)
  basinBuf <- VUM.unsafeNew rows
  activeSeen <- VUM.replicate rows (0 :: Word8)
  activeBuf <- VUM.unsafeNew rows
  pairSeen <- VUM.replicate edgeCount (0 :: Word8)
  pairBuf <- VUM.unsafeNew edgeCount
  pure
    HydrologyArena
      { haLakeCarry = carry
      , haPrevRelief = prevRelief
      , haDirtySeen = dirtySeen
      , haDirtyBuf = dirtyBuf
      , haBasinSeen = basinSeen
      , haBasinBuf = basinBuf
      , haActiveSeen = activeSeen
      , haActiveBuf = activeBuf
      , haPairSeen = pairSeen
      , haPairBuf = pairBuf
      }

writeHydrologyCarry :: HydrologyArena s -> ScalarField -> ST s ()
writeHydrologyCarry !arena !carry = do
  let !rows = VU.length carry
      go !i
        | i == rows = pure ()
        | otherwise = reliefWrite (haLakeCarry arena) i (VU.unsafeIndex carry i) >> go (i + 1)
  go 0

readHydrologyCarry :: HydrologyArena s -> ST s ScalarField
readHydrologyCarry !arena =
  reliefData <$> freezeReliefClone (haLakeCarry arena)

cacheHydrologyRelief :: HydrologyArena s -> ScalarField -> ST s ()
cacheHydrologyRelief !arena !relief = do
  let !rows = VU.length relief
      go !i
        | i == rows = pure ()
        | otherwise = reliefWrite (haPrevRelief arena) i (VU.unsafeIndex relief i) >> go (i + 1)
  go 0

clearMarks :: MWord8Vec s -> VU.Vector Int -> ST s ()
clearMarks !seen !items = do
  let !count = VU.length items
      go !ix
        | ix == count = pure ()
        | otherwise = do
            VUM.unsafeWrite seen (VU.unsafeIndex items ix) 0
            go (ix + 1)
  go 0

markInto :: MWord8Vec s -> MIntVec s -> STRef s Int -> Int -> ST s ()
markInto !seen !buf !countRef !i = do
  was <- VUM.unsafeRead seen i
  when (was == 0) $ do
    n <- readSTRef countRef
    VUM.unsafeWrite seen i 1
    VUM.unsafeWrite buf n i
    writeSTRef countRef (n + 1)
{-# INLINE markInto #-}

freezeBuf :: MIntVec s -> STRef s Int -> ST s (VU.Vector Int)
freezeBuf !buf !countRef = do
  n <- readSTRef countRef
  VU.generateM n (VUM.unsafeRead buf)

fullMorphogenicFrontier :: SolverEpoch -> TerrainMesh -> MorphogenicFrontier
fullMorphogenicFrontier !epoch !mesh =
  MorphogenicFrontier
      { mfRegion =
          fullDirectedEdgeRegionAt
            epoch
            ActiveRegionFullSweep
            (tmGraph mesh)
      , mfDirtyBasins = VU.empty
      }

collectDirtyFaces
  :: Double
  -> HydrologyArena s
  -> ScalarField
  -> ST s (VU.Vector Int)
collectDirtyFaces !eps !arena !relief = do
  let !rows = VU.length relief
  countRef <- newSTRef 0
  let go !i
        | i == rows = pure ()
        | otherwise = do
            oldZ <- reliefReadM (haPrevRelief arena) i
            let !newZ = VU.unsafeIndex relief i
            when (abs (newZ - oldZ) > eps) $
              markInto (haDirtySeen arena) (haDirtyBuf arena) countRef i
            go (i + 1)
  go 0
  dirty <- freezeBuf (haDirtyBuf arena) countRef
  clearMarks (haDirtySeen arena) dirty
  pure dirty

collectDirtyBasins
  :: HydrologyArena s
  -> Maybe HydrologyState
  -> VU.Vector Int
  -> ST s (VU.Vector Int)
collectDirtyBasins !arena !mPrev !dirtyFaces =
  case mPrev of
    Nothing -> pure VU.empty
    Just prev -> do
      countRef <- newSTRef 0
      let !labels = bsLakeSupernode (hsBasin prev)
          !count = VU.length dirtyFaces
          go !ix
            | ix == count = pure ()
            | otherwise = do
                let !i = VU.unsafeIndex dirtyFaces ix
                    !lab = VU.unsafeIndex labels i
                when (lab >= 0) $
                  markInto (haBasinSeen arena) (haBasinBuf arena) countRef lab
                go (ix + 1)
      go 0
      basins <- freezeBuf (haBasinBuf arena) countRef
      clearMarks (haBasinSeen arena) basins
      pure basins

expandRadius
  :: TerrainMesh
  -> Int
  -> MWord8Vec s
  -> MIntVec s
  -> STRef s Int
  -> ST s ()
expandRadius !mesh !radius !seen !buf !countRef = do
  let !gr = tmGraph mesh
      layer !frontLo !r
        | r <= 0 = pure ()
        | otherwise = do
            frontHi <- readSTRef countRef
            let faceLoop !ix
                  | ix == frontHi = pure ()
                  | otherwise = do
                      face <- VUM.unsafeRead buf ix
                      let (!lo, !hi) = edgeRange gr face
                          edgeLoop !e
                            | e == hi = pure ()
                            | otherwise = do
                                markInto seen buf countRef (VU.unsafeIndex (grNbrs gr) e)
                                edgeLoop (e + 1)
                      edgeLoop lo
                      faceLoop (ix + 1)
            faceLoop frontLo
            layer frontHi (r - 1)
  layer 0 radius

markDownstreamClosure
  :: TerrainMesh
  -> HydrologyArena s
  -> Maybe HydrologyState
  -> STRef s Int
  -> ST s ()
markDownstreamClosure !_mesh !arena !mPrev !countRef =
  case mPrev of
    Nothing -> pure ()
    Just prev -> do
      let !drain = hsDrainage prev
          !maxRecv = drMaxReceivers drain
          scan !front = do
            total <- readSTRef countRef
            if front >= total
              then pure ()
              else do
                let step !ix
                      | ix == total = pure ()
                      | otherwise = do
                          i <- VUM.unsafeRead (haActiveBuf arena) ix
                          let !count = VU.unsafeIndex (drReceiverCounts drain) i
                              edgeLoop !slot
                                | slot == count = pure ()
                                | otherwise = do
                                    let !j = VU.unsafeIndex (drReceiverIds drain) (slotIx maxRecv i slot)
                                    when (j >= 0) $
                                      markInto (haActiveSeen arena) (haActiveBuf arena) countRef j
                                    edgeLoop (slot + 1)
                          edgeLoop 0
                          step (ix + 1)
                step front
                scan total
      scan 0

buildActivePairs
  :: TerrainMesh
  -> HydrologyArena s
  -> VU.Vector Int
  -> ST s (VU.Vector Int)
buildActivePairs !mesh !arena !faces = do
  countRef <- newSTRef 0
  let !gr = tmGraph mesh
      !count = VU.length faces
      faceLoop !ix
        | ix == count = pure ()
        | otherwise = do
            let !i = VU.unsafeIndex faces ix
                (!lo, !hi) = edgeRange gr i
                edgeLoop !e
                  | e == hi = pure ()
                  | otherwise = do
                      markInto (haPairSeen arena) (haPairBuf arena) countRef e
                      edgeLoop (e + 1)
            edgeLoop lo
            faceLoop (ix + 1)
  faceLoop 0
  pairs <- freezeBuf (haPairBuf arena) countRef
  clearMarks (haPairSeen arena) pairs
  pure pairs

buildMorphogenicFrontier
  :: SolverEpoch
  -> TerrainMesh
  -> Double
  -> Int
  -> Double
  -> HydrologyArena s
  -> Maybe HydrologyState
  -> ScalarField
  -> ST s MorphogenicFrontier
buildMorphogenicFrontier !epoch !mesh !eps !radius !fullFrac !arena !mPrev !relief = do
  let !rows = tmRows mesh
  dirtyFaces <- collectDirtyFaces eps arena relief
  if VU.null dirtyFaces
    then pure MorphogenicFrontier
          { mfRegion = emptyActiveRegionAt epoch ActiveRegionMorphogenicDirtyRelief
          , mfDirtyBasins = VU.empty
          }
    else if fromIntegral (VU.length dirtyFaces) >= fullFrac * fromIntegral rows
      then pure (fullMorphogenicFrontier epoch mesh)
      else do
        dirtyBasins <- collectDirtyBasins arena mPrev dirtyFaces
        countRef <- newSTRef 0
        let seedDirty !ix
              | ix == VU.length dirtyFaces = pure ()
              | otherwise = do
                  markInto (haActiveSeen arena) (haActiveBuf arena) countRef (VU.unsafeIndex dirtyFaces ix)
                  seedDirty (ix + 1)
        seedDirty 0
        expandRadius mesh radius (haActiveSeen arena) (haActiveBuf arena) countRef
        case mPrev of
          Nothing -> pure ()
          Just prev -> do
            let !rows0 = hsRows prev
                !labs = bsLakeSupernode (hsBasin prev)
                !nBasins = VU.length dirtyBasins
                belongs !lab =
                  let go !k
                        | k == nBasins = False
                        | VU.unsafeIndex dirtyBasins k == lab = True
                        | otherwise = go (k + 1)
                  in go 0
                faceLoop !i
                  | i == rows0 = pure ()
                  | otherwise = do
                      let !lab = VU.unsafeIndex labs i
                      when (lab >= 0 && belongs lab) $
                        markInto (haActiveSeen arena) (haActiveBuf arena) countRef i
                      faceLoop (i + 1)
            faceLoop 0
        markDownstreamClosure mesh arena mPrev countRef
        faces <- freezeBuf (haActiveBuf arena) countRef
        clearMarks (haActiveSeen arena) faces
        pairs <- buildActivePairs mesh arena faces
        let !isFull = fromIntegral (VU.length faces) >= fullFrac * fromIntegral rows
        pure
          (if isFull then fullMorphogenicFrontier epoch mesh
           else MorphogenicFrontier
                 { mfRegion =
                     ActiveRegion
                       { arCoreFaces = dirtyFaces
                       , arHaloFaces = faces
                       , arSupport = pairs
                       , arIsFull = False
                       , arReason = ActiveRegionMorphogenicDirtyRelief
                       , arEpoch = epoch
                       }
                 , mfDirtyBasins = dirtyBasins
                 })

firstDrain :: Either DrainageError a -> Either HydrologyArenaError a
firstDrain = either (Left . HydrologyArenaDrainage) Right

basinTopologyStable
  :: BasinSurface
  -> BasinSurface
  -> VU.Vector Int
  -> Bool
basinTopologyStable !prev !next !faces =
  let !count = VU.length faces
      go !ix
        | ix == count = True
        | otherwise =
            let !i = VU.unsafeIndex faces ix
            in if VU.unsafeIndex (bsLakeSupernode prev) i /= VU.unsafeIndex (bsLakeSupernode next) i
                 then False
                 else if VU.unsafeIndex (bsLakeRepresentative prev) i /= VU.unsafeIndex (bsLakeRepresentative next) i
                   then False
                   else if VU.unsafeIndex (bsSpillFace prev) i /= VU.unsafeIndex (bsSpillFace next) i
                     then False
                     else if VU.unsafeIndex (bsSpillToFace prev) i /= VU.unsafeIndex (bsSpillToFace next) i
                       then False
                       else if VU.unsafeIndex (bsLakeOverflowMask prev) i /= VU.unsafeIndex (bsLakeOverflowMask next) i
                         then False
                         else go (ix + 1)
  in go 0
{-# INLINE basinTopologyStable #-}

patchDrainageFrontier
  :: DrainageLaw
  -> TerrainMesh
  -> DrainageInput
  -> DrainageResult
  -> VU.Vector Int
  -> ST s (Maybe DrainageResult)
patchDrainageFrontier !law !mesh !inp !prev !activeFaces = do
  let !rows = tmRows mesh
      !maxRecv = drMaxReceivers prev
      !gr = tmGraph mesh
      !headF = diPuissance inp
      !edgeLens = tmEdgeLengths mesh
      !eps = dlFillEpsilon law
      !p = dlSlopePower law
      !minDist = dlMinDistance law
      !flatSlope = dlFlatSlope law
      !countActive = VU.length activeFaces
  recvCounts <- VU.thaw (drReceiverCounts prev)
  recvIds <- VU.thaw (drReceiverIds prev)
  recvWeights <- VU.thaw (drReceiverWeights prev)
  recvSlopes <- VU.thaw (drReceiverSlopes prev)
  domRecv <- VU.thaw (drDominantReceiver prev)
  faceSlope <- VU.thaw (drFaceSlope prev)
  indeg <- VUM.replicate rows (0 :: Int)
  areaAcc <- VUM.replicate rows 0.0
  discharge <- VUM.replicate rows 0.0
  queue <- VUM.unsafeNew rows
  order <- VUM.unsafeNew rows
  failedRef <- newSTRef False

  let clearRow !i =
        let !base = i * maxRecv
            go !slot
              | slot == maxRecv = pure ()
              | otherwise = do
                  let !ix = base + slot
                  VUM.unsafeWrite recvIds ix (-1)
                  VUM.unsafeWrite recvWeights ix 0.0
                  VUM.unsafeWrite recvSlopes ix 0.0
                  go (slot + 1)
        in go 0

      patchFace !i = do
        clearRow i
        if isOutletAt (diOutletMask inp) i
          then do
            VUM.unsafeWrite recvCounts i 0
            VUM.unsafeWrite domRecv i (-1)
            VUM.unsafeWrite faceSlope i 0.0
          else do
            let !hI = VU.unsafeIndex headF i
                (!lo, !hi) = edgeRange gr i
                scan !e !count !sumBasis !bestJ !bestSlope
                  | e == hi = pure (count, sumBasis, bestJ, bestSlope)
                  | otherwise =
                      let !j = VU.unsafeIndex (grNbrs gr) e
                          !dropH = hI - VU.unsafeIndex headF j
                      in if dropH <= eps
                           then scan (e + 1) count sumBasis bestJ bestSlope
                           else
                             let !dist = max minDist (VU.unsafeIndex edgeLens e)
                                 !slope = dropH / dist
                                 !basis = positivePow p slope
                                 !bestJ1 = if slope > bestSlope then j else bestJ
                                 !bestS1 = max bestSlope slope
                             in scan (e + 1) (count + 1) (sumBasis + basis) bestJ1 bestS1
                emit !e !slot !sumBasis
                  | e == hi = VUM.unsafeWrite recvCounts i slot
                  | otherwise =
                      let !j = VU.unsafeIndex (grNbrs gr) e
                          !dropH = hI - VU.unsafeIndex headF j
                      in if dropH <= eps
                           then emit (e + 1) slot sumBasis
                           else do
                             let !dist = max minDist (VU.unsafeIndex edgeLens e)
                                 !slope = dropH / dist
                                 !basis = positivePow p slope
                                 !w = if sumBasis <= 0.0 then 0.0 else basis / sumBasis
                                 !ix = slotIx maxRecv i slot
                             VUM.unsafeWrite recvIds ix j
                             VUM.unsafeWrite recvWeights ix w
                             VUM.unsafeWrite recvSlopes ix slope
                             emit (e + 1) (slot + 1) sumBasis
            (countDown, sumBasis, bestJ, bestSlope) <- scan lo (0 :: Int) 0.0 (-1) 0.0
            if countDown > 0
              then do
                VUM.unsafeWrite domRecv i bestJ
                VUM.unsafeWrite faceSlope i bestSlope
                emit lo 0 sumBasis
              else do
                let !parentI = VU.unsafeIndex (drSpillParent prev) i
                if parentI >= 0
                  then do
                    let !dist = max minDist (faceDistance gr i parentI)
                        !slope = max flatSlope ((hI - VU.unsafeIndex headF parentI) / dist)
                    if maxRecv > 0
                      then do
                        let !ix = slotIx maxRecv i 0
                        VUM.unsafeWrite recvCounts i 1
                        VUM.unsafeWrite recvIds ix parentI
                        VUM.unsafeWrite recvWeights ix 1.0
                        VUM.unsafeWrite recvSlopes ix slope
                        VUM.unsafeWrite domRecv i parentI
                        VUM.unsafeWrite faceSlope i slope
                      else writeSTRef failedRef True
                  else do
                    let !isClosedLake = VU.unsafeIndex (diOutletMask inp) i /= 0
                    if isClosedLake
                      then do
                        VUM.unsafeWrite recvCounts i 0
                        VUM.unsafeWrite domRecv i (-1)
                        VUM.unsafeWrite faceSlope i 0.0
                      else writeSTRef failedRef True

  let faceLoop !ix
        | ix == countActive = pure ()
        | otherwise = patchFace (VU.unsafeIndex activeFaces ix) >> faceLoop (ix + 1)
  faceLoop 0
  failed <- readSTRef failedRef
  if failed
    then pure Nothing
    else do
      let indegLoop !i
            | i == rows = pure ()
            | otherwise = do
                c <- VUM.unsafeRead recvCounts i
                let recvLoop !slot
                      | slot == c = pure ()
                      | otherwise = do
                          let !jIx = slotIx maxRecv i slot
                          j <- VUM.unsafeRead recvIds jIx
                          when (j >= 0) $ do
                            old <- VUM.unsafeRead indeg j
                            VUM.unsafeWrite indeg j (old + 1)
                          recvLoop (slot + 1)
                recvLoop 0
                indegLoop (i + 1)
      indegLoop 0

      let initLoop !i
            | i == rows = pure ()
            | otherwise = do
                let !a0 = VU.unsafeIndex (diFaceArea inp) i
                    !q0 = a0 * VU.unsafeIndex (diRunoff inp) i
                VUM.unsafeWrite areaAcc i a0
                VUM.unsafeWrite discharge i q0
                initLoop (i + 1)
      initLoop 0

      let seedQueue !i !tailIx
            | i == rows = pure tailIx
            | otherwise = do
                d <- VUM.unsafeRead indeg i
                if d == 0
                  then VUM.unsafeWrite queue tailIx i >> seedQueue (i + 1) (tailIx + 1)
                  else seedQueue (i + 1) tailIx

          push !i !slot !count !qHere !aHere !tailIx
            | slot == count = pure tailIx
            | otherwise = do
                let !ix = slotIx maxRecv i slot
                j <- VUM.unsafeRead recvIds ix
                if j < 0
                  then push i (slot + 1) count qHere aHere tailIx
                  else do
                    w <- VUM.unsafeRead recvWeights ix
                    oldQ <- VUM.unsafeRead discharge j
                    oldA <- VUM.unsafeRead areaAcc j
                    VUM.unsafeWrite discharge j (oldQ + w * qHere)
                    VUM.unsafeWrite areaAcc j (oldA + w * aHere)
                    d <- VUM.unsafeRead indeg j
                    let !d1 = d - 1
                    VUM.unsafeWrite indeg j d1
                    tailIx1 <-
                      if d1 == 0
                        then VUM.unsafeWrite queue tailIx j >> pure (tailIx + 1)
                        else pure tailIx
                    push i (slot + 1) count qHere aHere tailIx1

          topoLoop !headIx !tailIx !done
            | headIx == tailIx = pure done
            | otherwise = do
                i <- VUM.unsafeRead queue headIx
                VUM.unsafeWrite order done i
                qHere <- VUM.unsafeRead discharge i
                aHere <- VUM.unsafeRead areaAcc i
                c <- VUM.unsafeRead recvCounts i
                tailIx1 <- push i 0 c qHere aHere tailIx
                topoLoop (headIx + 1) tailIx1 (done + 1)

      tail0 <- seedQueue 0 0
      done <- topoLoop 0 tail0 0
      if done /= rows
        then pure Nothing
        else do
          recvCountsF <- VU.unsafeFreeze recvCounts
          recvIdsF <- VU.unsafeFreeze recvIds
          recvWeightsF <- VU.unsafeFreeze recvWeights
          recvSlopesF <- VU.unsafeFreeze recvSlopes
          domRecvF <- VU.unsafeFreeze domRecv
          faceSlopeF <- VU.unsafeFreeze faceSlope
          areaAccF <- VU.unsafeFreeze areaAcc
          dischargeF <- VU.unsafeFreeze discharge
          orderF <- VU.unsafeFreeze order
          pure $
            Just
              DrainageResult
                { drRows = rows
                , drMaxReceivers = maxRecv
                , drHydraulicHead = headF
                , drSpillParent = drSpillParent prev
                , drReceiverCounts = recvCountsF
                , drReceiverIds = recvIdsF
                , drReceiverWeights = recvWeightsF
                , drReceiverSlopes = recvSlopesF
                , drDominantReceiver = domRecvF
                , drFaceSlope = faceSlopeF
                , drContribArea = areaAccF
                , drDischarge = dischargeF
                , drTopologicalOrder = orderF
                }

buildHydrologyState
  :: MorphogenicFrontier
  -> BasinSurface
  -> ScalarField
  -> OutletMask
  -> DrainageResult
  -> WaterSurfaceResult
  -> HydrologyState
buildHydrologyState !frontier !basin !hydElev !hydOut !drain !water =
  HydrologyState
    { hsRows = drRows drain
    , hsBasin = basin
    , hsWater = water
    , hsHydraulicElevation = hydElev
    , hsHydraulicOutletMask = hydOut
    , hsDrainage = drain
    , hsDirtyFaces = arCoreFaces (mfRegion frontier)
    , hsDirtyBasins = mfDirtyBasins frontier
    , hsActiveFaces = arHaloFaces (mfRegion frontier)
    , hsActivePairs = arSupport (mfRegion frontier)
    }

advanceHydrologyArena
  :: HydrologyMode
  -> Bool
  -> MorphogenicLaw
  -> TerrainMesh
  -> DrainageInput
  -> HydrologyArena s
  -> Maybe HydrologyState
  -> MorphogenicFrontier
  -> ST s (Either HydrologyArenaError HydrologyState)
advanceHydrologyArena !mode !allowLocal !law !mesh !inp !arena !mPrev !frontier = do
  carry <- readHydrologyCarry arena
  let riverDrainE =
        case mPrev of
          Just prev
            | allowLocal && not (arIsFull (mfRegion frontier)) -> Right (Just (hsDrainage prev))
          _ ->
            case drainFixedTopography (mglDrainageLaw law) mesh inp of
              Left err -> Left (HydrologyArenaDrainage err)
              Right dr -> Right (Just dr)
  case riverDrainE of
    Left err -> pure (Left err)
    Right mRiverDrain -> do
      let basinE =
            case mode of
              HydrologyAdvance -> advanceBasinSurface (mglBasinStageLaw law) mesh inp carry mRiverDrain
              HydrologyProject -> projectBasinSurface (mglBasinStageLaw law) mesh inp carry mRiverDrain
      case basinE of
        Left err -> pure (Left (HydrologyArenaWater err))
        Right basin -> do
          let !hydElev = buildHydraulicElevation (mglLakeFeedbackLaw law) inp basin
              !hydOut = buildHydraulicOutletMask inp basin
              !hydInp =
                inp
                  { diPuissance = hydElev
                  , diOutletMask = hydOut
                  , diBasinTerminalMask = VU.replicate (tmRows mesh) 0
                  }
              localStable =
                case mPrev of
                  Nothing -> False
                  Just prev ->
                    allowLocal
                      && not (arIsFull (mfRegion frontier))
                      && basinTopologyStable (hsBasin prev) basin (arHaloFaces (mfRegion frontier))
          drainE <-
            if not localStable || VU.null (arHaloFaces (mfRegion frontier))
              then pure (firstDrain (drainFixedTopography (mglDrainageLaw law) mesh hydInp))
              else do
                patched <- case mPrev of
                  Nothing -> pure Nothing
                  Just prev -> patchDrainageFrontier (mglDrainageLaw law) mesh hydInp (hsDrainage prev) (arHaloFaces (mfRegion frontier))
                case patched of
                  Nothing -> pure (firstDrain (drainFixedTopography (mglDrainageLaw law) mesh hydInp))
                  Just dr -> pure (Right dr)
          case drainE of
            Left err -> pure (Left err)
            Right drain ->
              case buildWaterSurfaceFromBasin (mglChannelLaw law) (mglChannelStageLaw law) mesh inp drain basin of
                Left err -> pure (Left (HydrologyArenaWater err))
                Right water -> pure (Right (buildHydrologyState frontier basin hydElev hydOut drain water))
