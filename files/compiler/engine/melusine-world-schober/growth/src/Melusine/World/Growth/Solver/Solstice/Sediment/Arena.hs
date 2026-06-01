{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.World.Growth.Solver.Solstice.Sediment.Arena
  ( SedimentState(..)
  , SedimentArena(..)
  , newSedimentArena
  , snapshotSedimentState
  , edgeExnerStepActive
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
  ( DrainageResult(..)
  , OutletMask
  )
import Melusine.World.Growth.Solver.Common.MeshOps (isOutletAt)
import Melusine.World.Growth.Solver.Common.Relief
  ( MReliefBlock
  , freezeReliefClone
  , newReliefMutable
  , reliefAdd
  , reliefData
  , reliefReadM
  , reliefWrite
  )
import Melusine.World.Growth.Solver.Common.Util (MDoubleVec, clamp01, isFiniteD, positivePow)
import Melusine.World.Growth.Solver.Solstice.Engine.Common (channelUnitDischargeAt, edgeDistanceToReceiver)
import Melusine.World.Growth.Solver.Solstice.Laws.Relief
  ( ExplicitExnerLaw(..)
  , LacustrineSettlingLaw(..)
  , LakeFeedbackLaw(..)
  , SedimentTransportLaw(..)
  )
import Melusine.World.Growth.Solver.Common.TerrainMesh
  ( TerrainMesh(..)
  , slotIx
  )
import Melusine.World.Growth.Solver.Solstice.WaterSurface
  ( BasinSurface(..)
  , WaterSurfaceResult(..)
  )

type SedimentState :: Type
data SedimentState = SedimentState
  { ssBedCapacity         :: !ScalarField
  , ssBedLoad             :: !ScalarField
  , ssBedErosion          :: !ScalarField
  , ssBedDeposition       :: !ScalarField
  , ssSuspendedCapacity   :: !ScalarField
  , ssSuspendedLoad       :: !ScalarField
  , ssSuspendedErosion    :: !ScalarField
  , ssSuspendedDeposition :: !ScalarField
  , ssLakeDeltaDeposition :: !ScalarField
  , ssLakeSettling        :: !ScalarField
  , ssLakeSuspendedCarry  :: !ScalarField
  }

type SedimentArena :: Type -> Type
data SedimentArena s = SedimentArena
  { seaBedCapacity         :: !(MReliefBlock s)
  , seaBedLoad             :: !(MReliefBlock s)
  , seaBedErosion          :: !(MReliefBlock s)
  , seaBedDeposition       :: !(MReliefBlock s)
  , seaSuspendedCapacity   :: !(MReliefBlock s)
  , seaSuspendedLoad       :: !(MReliefBlock s)
  , seaSuspendedErosion    :: !(MReliefBlock s)
  , seaSuspendedDeposition :: !(MReliefBlock s)
  , seaLakeDeltaDeposition :: !(MReliefBlock s)
  , seaLakeSettling        :: !(MReliefBlock s)
  , seaLakeSuspendedCarry  :: !(MReliefBlock s)
  , seaLakeIncomingSusp    :: !(MDoubleVec s)
  , seaLakeWaterArea       :: !(MDoubleVec s)
  , seaLakeSettlingNum     :: !(MDoubleVec s)
  , seaLakeCarryWork       :: !(MDoubleVec s)
  }

solidFaceArea :: ExplicitExnerLaw -> ScalarField -> Int -> Double
solidFaceArea !law !faceArea !i =
  max 1.0e-12 ((1.0 - exlPorosity law) * VU.unsafeIndex faceArea i)
{-# INLINE solidFaceArea #-}

lakeCapacityFactorAt :: LakeFeedbackLaw -> WaterSurfaceResult -> Int -> Double
lakeCapacityFactorAt !law !water !i =
  if VU.unsafeIndex (wsLakeMask water) i /= 0 then lflWetCapacityFactor law else 1.0
{-# INLINE lakeCapacityFactorAt #-}

addVecAt :: MDoubleVec s -> Int -> Double -> ST s ()
addVecAt !mv !i !dx = do
  x0 <- VUM.unsafeRead mv i
  VUM.unsafeWrite mv i (x0 + dx)
{-# INLINE addVecAt #-}

clearBlock :: MReliefBlock s -> Int -> ST s ()
clearBlock !blk !rows =
  let go !i
        | i == rows = pure ()
        | otherwise = reliefWrite blk i 0.0 >> go (i + 1)
  in go 0

clearVec :: MDoubleVec s -> Int -> ST s ()
clearVec !mv !n =
  let go !i
        | i == n = pure ()
        | otherwise = VUM.unsafeWrite mv i 0.0 >> go (i + 1)
  in go 0

updateBest :: STRef s Double -> Double -> ST s ()
updateBest !bestRef !dz = do
  let !d = abs dz
  old <- readSTRef bestRef
  when (d > old) (writeSTRef bestRef d)
{-# INLINE updateBest #-}

seedSedimentFromIncision
  :: ExplicitExnerLaw
  -> ScalarField
  -> ScalarField
  -> VU.Vector Word8
  -> MReliefBlock s
  -> MReliefBlock s
  -> MReliefBlock s
  -> ST s ()
seedSedimentFromIncision !law !faceArea !before !activeMask !bed !bedLoad !suspLoad =
  let !rows = VU.length before
      !fracF = exlSuspendedFraction law
      go !i
        | i == rows = pure ()
        | otherwise = do
            oldSusp <- reliefReadM suspLoad i
            if VU.unsafeIndex activeMask i == 0
              then do
                reliefWrite bedLoad i 0.0
                reliefWrite suspLoad i oldSusp
                go (i + 1)
              else do
                z1 <- reliefReadM bed i
                let !solidA = solidFaceArea law faceArea i
                    !srcV = max 0.0 (VU.unsafeIndex before i - z1) * solidA
                    !fSusp = clamp01 (VU.unsafeIndex fracF i)
                reliefWrite bedLoad i (srcV * (1.0 - fSusp))
                reliefWrite suspLoad i (oldSusp + srcV * fSusp)
                go (i + 1)
  in go 0

receiverTargetElevation :: WaterSurfaceResult -> ScalarField -> Int -> Double
receiverTargetElevation !water !bed !j =
  let !s = VU.unsafeIndex (wsTideSurface water) j
  in if isFiniteD s then s else VU.unsafeIndex bed j
{-# INLINE receiverTargetElevation #-}

edgeModeCapacity
  :: ExplicitExnerLaw
  -> SedimentTransportLaw
  -> Double
  -> LakeFeedbackLaw
  -> WaterSurfaceResult
  -> TerrainMesh
  -> Int
  -> Int
  -> Double
  -> Double
  -> Double
  -> Double
edgeModeCapacity !exLaw !mode !stageMul !lakeLaw !water !mesh !i !j !qEdge !zI !zJ =
  let !dist = edgeDistanceToReceiver mesh 1.0e-12 i j
      !dropH = zI - zJ
      !slopeRaw = if dropH <= 0.0 then 0.0 else dropH / dist
      !slope = if slopeRaw <= 0.0 then 0.0 else max (stlMinSlope mode) slopeRaw
  in if qEdge <= 0.0 || slope <= 0.0
       then 0.0
       else
         stageMul
         * exlDt exLaw
         * VU.unsafeIndex (stlCapacityCoeff mode) i
         * lakeCapacityFactorAt lakeLaw water i
         * positivePow (stlAreaExponent mode) qEdge
         * positivePow (stlSlopeExponent mode) slope
{-# INLINE edgeModeCapacity #-}

depositSolidVolumeAtFace
  :: ExplicitExnerLaw
  -> Double
  -> Bool
  -> OutletMask
  -> TerrainMesh
  -> ScalarField
  -> Int
  -> Double
  -> MReliefBlock s
  -> MReliefBlock s
  -> STRef s Double
  -> ST s ()
depositSolidVolumeAtFace !_law !_spreadScale !_fixOutlets !_outMask !_mesh !_faceArea !_i !_vol !_bed !_dep !_bestRef =
  pure ()
{-# INLINE depositSolidVolumeAtFace #-}

buildLakeMembers
  :: Int
  -> ScalarField
  -> ScalarField
  -> WaterSurfaceResult
  -> SedimentArena s
  -> ST s (VU.Vector Int, VU.Vector Int)
buildLakeMembers !rows !faceArea !settlingVel !water !arena = do
  counts <- VUM.replicate rows (0 :: Int)
  clearVec (seaLakeWaterArea arena) rows
  clearVec (seaLakeSettlingNum arena) rows
  let !lakeIdF = wsLakeSupernode water
      !lakeMask = wsLakeMask water
      countLoop !i !total
        | i == rows = pure total
        | otherwise =
            let !r = VU.unsafeIndex lakeIdF i
            in if r < 0 || VU.unsafeIndex lakeMask i == 0
                 then countLoop (i + 1) total
                 else do
                   c <- VUM.unsafeRead counts r
                   VUM.unsafeWrite counts r (c + 1)
                   let !a = VU.unsafeIndex faceArea i
                   addVecAt (seaLakeWaterArea arena) r a
                   addVecAt (seaLakeSettlingNum arena) r (a * VU.unsafeIndex settlingVel i)
                   countLoop (i + 1) (total + 1)
  total <- countLoop 0 0
  offs <- VUM.replicate (rows + 1) 0
  let prefix !r !acc
        | r == rows = VUM.unsafeWrite offs rows acc
        | otherwise = do
            VUM.unsafeWrite offs r acc
            c <- VUM.unsafeRead counts r
            prefix (r + 1) (acc + c)
  prefix 0 0
  cursor <- VUM.unsafeNew rows
  let copyCursor !r
        | r == rows = pure ()
        | otherwise = do
            x <- VUM.unsafeRead offs r
            VUM.unsafeWrite cursor r x
            copyCursor (r + 1)
  copyCursor 0
  members <- VUM.unsafeNew total
  let fill !i
        | i == rows = pure ()
        | otherwise =
            let !r = VU.unsafeIndex lakeIdF i
            in if r < 0 || VU.unsafeIndex lakeMask i == 0
                 then fill (i + 1)
                 else do
                   pos <- VUM.unsafeRead cursor r
                   VUM.unsafeWrite members pos i
                   VUM.unsafeWrite cursor r (pos + 1)
                   fill (i + 1)
  fill 0
  offsF <- VU.unsafeFreeze offs
  membersF <- VU.unsafeFreeze members
  pure (offsF, membersF)

edgeExnerStepActive
  :: ExplicitExnerLaw
  -> Double
  -> LakeFeedbackLaw
  -> Bool
  -> ScalarField
  -> ScalarField
  -> OutletMask
  -> TerrainMesh
  -> DrainageResult
  -> BasinSurface
  -> WaterSurfaceResult
  -> VU.Vector Word8
  -> ScalarField
  -> MReliefBlock s
  -> SedimentArena s
  -> ST s Double
edgeExnerStepActive !law !stageMul !lakeLaw !fixOutlets !outletBase !faceArea !outMask !mesh !drain !basin !water !activeMask !before !bed !arena = do
  let !rows = drRows drain
      !order = drTopologicalOrder drain
      !maxRecv = drMaxReceivers drain
      !lakeIdF = wsLakeSupernode water
      !repFaceF = wsLakeRepresentative water
      !lakeMaskF = wsLakeMask water
      !settleLaw = exlSettlingLaw law
      !settVelF = lslSettlingVelocity settleLaw
      !deltaSpread = lslInletSpread settleLaw
      !channelSpread = 0.5 * lslInletSpread settleLaw
      !heldVolById = bsSupernodeHeldVolumeById basin
      !repById = bsSupernodeRepresentativeById basin
      !exitToById = bsSupernodeExitToFaceById basin

  clearBlock (seaBedCapacity arena) rows
  clearBlock (seaBedErosion arena) rows
  clearBlock (seaBedDeposition arena) rows
  clearBlock (seaSuspendedCapacity arena) rows
  clearBlock (seaSuspendedErosion arena) rows
  clearBlock (seaSuspendedDeposition arena) rows
  clearBlock (seaLakeDeltaDeposition arena) rows
  clearBlock (seaLakeSettling arena) rows
  clearBlock (seaBedLoad arena) rows
  clearBlock (seaSuspendedLoad arena) rows
  clearVec (seaLakeIncomingSusp arena) rows
  clearVec (seaLakeCarryWork arena) rows

  if rows <= 0
    then pure 0.0
    else do
      bestRef <- newSTRef 0.0
      (lakeOffs, lakeMembers) <- buildLakeMembers rows faceArea settVelF water arena

      let aggregateCarry !i
            | i == rows = pure ()
            | otherwise = do
                oldCarry <- reliefReadM (seaLakeSuspendedCarry arena) i
                reliefWrite (seaLakeSuspendedCarry arena) i 0.0
                let !r = VU.unsafeIndex lakeIdF i
                if oldCarry <= 0.0
                  then aggregateCarry (i + 1)
                  else if r >= 0 && VU.unsafeIndex lakeMaskF i /= 0
                    then do
                      addVecAt (seaLakeCarryWork arena) r oldCarry
                      aggregateCarry (i + 1)
                    else do
                      reliefAdd (seaSuspendedLoad arena) i oldCarry
                      aggregateCarry (i + 1)
      aggregateCarry 0

      seedSedimentFromIncision law faceArea before activeMask bed (seaBedLoad arena) (seaSuspendedLoad arena)

      let processLakeRepresentative !i !r = do
            loadBedLocal <- reliefReadM (seaBedLoad arena) i
            loadSuspLocal <- reliefReadM (seaSuspendedLoad arena) i
            depositSolidVolumeAtFace
              law
              deltaSpread
              fixOutlets
              outMask
              mesh
              faceArea
              i
              loadBedLocal
              bed
              (seaLakeDeltaDeposition arena)
              bestRef
            let !mouthSusp = lslInletFraction settleLaw * loadSuspLocal
                !bulkSusp = max 0.0 (loadSuspLocal - mouthSusp)
            depositSolidVolumeAtFace
              law
              deltaSpread
              fixOutlets
              outMask
              mesh
              faceArea
              i
              mouthSusp
              bed
              (seaLakeDeltaDeposition arena)
              bestRef
            addVecAt (seaLakeIncomingSusp arena) r bulkSusp
            reliefWrite (seaBedLoad arena) i 0.0
            reliefWrite (seaSuspendedLoad arena) i 0.0

            incoming <- VUM.unsafeRead (seaLakeIncomingSusp arena) r
            carryPrev <- VUM.unsafeRead (seaLakeCarryWork arena) r
            areaWet <- VUM.unsafeRead (seaLakeWaterArea arena) r
            settleNum <- VUM.unsafeRead (seaLakeSettlingNum arena) r
            let !heldVol = VU.unsafeIndex heldVolById r
                !rep = VU.unsafeIndex repById r
                !exitTo = VU.unsafeIndex exitToById r
                !meanSett = if areaWet <= 0.0 then 0.0 else settleNum / areaWet
                !qOut =
                  if rep < 0
                    then 0.0
                    else if exitTo >= 0 || isOutletAt outMask rep
                      then max 0.0 (VU.unsafeIndex (drDischarge drain) rep)
                      else 0.0
                !volEff =
                  max heldVol
                    (max (areaWet * lslMinWaterDepth settleLaw)
                         (qOut * lslMinResidenceTime settleLaw))
                !m0 = max 0.0 (incoming + carryPrev)
                !kSet = if meanSett <= 0.0 || areaWet <= 0.0 then 0.0 else meanSett * areaWet / max 1.0e-12 volEff
                !kOut = if qOut <= 0.0 then 0.0 else qOut / max 1.0e-12 volEff
                !theta = exlDt law * (kSet + kOut)
                (!deposited0, !exported0, !_carry0) =
                  if m0 <= 0.0 || theta <= 0.0
                    then (0.0, 0.0, m0)
                    else
                      let !lost = m0 * (1.0 - exp (-theta))
                          !kTot = kSet + kOut
                          !dep = if kTot <= 0.0 then 0.0 else lost * (kSet / kTot)
                          !out = if kTot <= 0.0 then 0.0 else lost * (kOut / kTot)
                          !remaining = max 0.0 (m0 - dep - out)
                      in (dep, out, remaining)
                !deposited = min (lslTrapEfficiencyCap settleLaw * m0) deposited0
                !exported = min (max 0.0 (m0 - deposited)) exported0
                !carryNext = max 0.0 (m0 - deposited - exported)
                !lo = VU.unsafeIndex lakeOffs r
                !hi = VU.unsafeIndex lakeOffs (r + 1)

                distributeSettling !ix !den
                  | ix == hi = pure ()
                  | otherwise = do
                      let !f = VU.unsafeIndex lakeMembers ix
                          !a = VU.unsafeIndex faceArea f
                          !depth = max (lslMinWaterDepth settleLaw) (VU.unsafeIndex (wsLakeStoredVolume water) f / max 1.0e-12 a)
                          !w = a / max 1.0e-12 (positivePow (lslShallowingPower settleLaw) depth)
                          !depF = if den <= 0.0 then 0.0 else deposited * (w / den)
                          !volF = VU.unsafeIndex (wsLakeStoredVolume water) f
                          !carryF =
                            if heldVol > 0.0
                              then carryNext * (volF / heldVol)
                              else 0.0
                      depositSolidVolumeAtFace
                        law
                        0.0
                        fixOutlets
                        outMask
                        mesh
                        faceArea
                        f
                        depF
                        bed
                        (seaLakeSettling arena)
                        bestRef
                      reliefWrite (seaLakeSuspendedCarry arena) f carryF
                      distributeSettling (ix + 1) den

                weightDen !ix !acc
                  | ix == hi = pure acc
                  | otherwise = do
                      let !f = VU.unsafeIndex lakeMembers ix
                          !a = VU.unsafeIndex faceArea f
                          !depth = max (lslMinWaterDepth settleLaw) (VU.unsafeIndex (wsLakeStoredVolume water) f / max 1.0e-12 a)
                          !w = a / max 1.0e-12 (positivePow (lslShallowingPower settleLaw) depth)
                      weightDen (ix + 1) (acc + w)
            den <- weightDen lo 0.0
            distributeSettling lo den

            if exported > 0.0 && rep >= 0
              then if exitTo >= 0
                then
                  let !downLake = VU.unsafeIndex lakeIdF exitTo
                  in if downLake >= 0 && VU.unsafeIndex lakeMaskF exitTo /= 0
                       then addVecAt (seaLakeIncomingSusp arena) downLake exported
                       else reliefAdd (seaSuspendedLoad arena) exitTo exported
                else pure ()
              else pure ()

          processNonRepresentativeLakeFace !i !r = do
            loadBedLocal <- reliefReadM (seaBedLoad arena) i
            loadSuspLocal <- reliefReadM (seaSuspendedLoad arena) i
            depositSolidVolumeAtFace
              law
              deltaSpread
              fixOutlets
              outMask
              mesh
              faceArea
              i
              loadBedLocal
              bed
              (seaLakeDeltaDeposition arena)
              bestRef
            let !mouthSusp = lslInletFraction settleLaw * loadSuspLocal
                !bulkSusp = max 0.0 (loadSuspLocal - mouthSusp)
            depositSolidVolumeAtFace
              law
              deltaSpread
              fixOutlets
              outMask
              mesh
              faceArea
              i
              mouthSusp
              bed
              (seaLakeDeltaDeposition arena)
              bestRef
            addVecAt (seaLakeIncomingSusp arena) r bulkSusp
            reliefWrite (seaBedLoad arena) i 0.0
            reliefWrite (seaSuspendedLoad arena) i 0.0

          processRiverFace !i = do
            let !q0 = VU.unsafeIndex (drDischarge drain) i
                !qBedFace = channelUnitDischargeAt water q0 i
                !count = VU.unsafeIndex (drReceiverCounts drain) i
                !solidA = solidFaceArea law faceArea i
                !fSusp = clamp01 (VU.unsafeIndex (exlSuspendedFraction law) i)
                !bedLaw = exlBedloadLaw law
                !suspLaw = exlSuspendedLaw law
            z0 <- reliefReadM bed i
            if fixOutlets && isOutletAt outMask i
              then do
                let !zFix = VU.unsafeIndex outletBase i
                reliefWrite bed i zFix
                updateBest bestRef (zFix - z0)
              else do
                loadBedIn <- reliefReadM (seaBedLoad arena) i
                loadSuspIn <- reliefReadM (seaSuspendedLoad arena) i
                let slotLoop !slot !capBedTot !capSuspTot !zMeanNum !wSum
                      | slot == count = pure (capBedTot, capSuspTot, if wSum <= 0.0 then z0 else zMeanNum / wSum)
                      | otherwise = do
                          let !ix = slotIx maxRecv i slot
                              !j = VU.unsafeIndex (drReceiverIds drain) ix
                              !wWater = VU.unsafeIndex (drReceiverWeights drain) ix
                              !qBedEdge = qBedFace * wWater
                              !qSuspEdge = q0 * wWater
                          if j < 0 || (qBedEdge <= 0.0 && qSuspEdge <= 0.0)
                            then slotLoop (slot + 1) capBedTot capSuspTot zMeanNum wSum
                            else do
                              let !zT = receiverTargetElevation water before j
                                  !capBed = edgeModeCapacity law bedLaw stageMul lakeLaw water mesh i j qBedEdge z0 zT
                                  !capSusp = edgeModeCapacity law suspLaw stageMul lakeLaw water mesh i j qSuspEdge z0 zT
                              slotLoop
                                (slot + 1)
                                (capBedTot + capBed)
                                (capSuspTot + capSusp)
                                (zMeanNum + wWater * zT)
                                (wSum + wWater)
                (capBedTot, capSuspTot, zMean) <- slotLoop 0 0.0 0.0 0.0 0.0
                let !availDepth = exlMaxReliefFraction law * max 0.0 (z0 - zMean)
                    !availVol = solidA * availDepth
                    !availBed = availVol * (1.0 - fSusp)
                    !availSusp = availVol * fSusp
                    !defBed = max 0.0 (capBedTot - loadBedIn)
                    !defSusp = max 0.0 (capSuspTot - loadSuspIn)
                    !erodedBed = min availBed defBed
                    !erodedSusp = min availSusp defSusp
                    !bedMid = loadBedIn + erodedBed
                    !suspMid = loadSuspIn + erodedSusp
                    !depositedBed = if count <= 0 then bedMid else max 0.0 (bedMid - capBedTot)
                    !depositedSusp = if count <= 0 then suspMid else max 0.0 (suspMid - capSuspTot)
                    !bedOut = bedMid - depositedBed
                    !suspOut = suspMid - depositedSusp
                    !erosionVol = erodedBed + erodedSusp
                    !zEroded = z0 - erosionVol / solidA
                reliefWrite bed i zEroded
                updateBest bestRef (zEroded - z0)
                reliefWrite (seaBedCapacity arena) i capBedTot
                reliefWrite (seaSuspendedCapacity arena) i capSuspTot
                reliefWrite (seaBedLoad arena) i bedOut
                reliefWrite (seaSuspendedLoad arena) i suspOut
                reliefWrite (seaBedErosion arena) i erodedBed
                reliefWrite (seaSuspendedErosion arena) i erodedSusp
                depositSolidVolumeAtFace
                  law
                  channelSpread
                  fixOutlets
                  outMask
                  mesh
                  faceArea
                  i
                  depositedBed
                  bed
                  (seaBedDeposition arena)
                  bestRef
                depositSolidVolumeAtFace
                  law
                  channelSpread
                  fixOutlets
                  outMask
                  mesh
                  faceArea
                  i
                  depositedSusp
                  bed
                  (seaSuspendedDeposition arena)
                  bestRef

                let pushLoop !slot
                      | slot == count = pure ()
                      | otherwise = do
                          let !ix = slotIx maxRecv i slot
                              !j = VU.unsafeIndex (drReceiverIds drain) ix
                              !wWater = VU.unsafeIndex (drReceiverWeights drain) ix
                              !qBedEdge = qBedFace * wWater
                              !qSuspEdge = q0 * wWater
                          if j < 0 || (qBedEdge <= 0.0 && qSuspEdge <= 0.0)
                            then pushLoop (slot + 1)
                            else do
                              zSrc <- reliefReadM bed i
                              let !zT = receiverTargetElevation water before j
                                  !capBed = edgeModeCapacity law bedLaw stageMul lakeLaw water mesh i j qBedEdge zSrc zT
                                  !capSusp = edgeModeCapacity law suspLaw stageMul lakeLaw water mesh i j qSuspEdge zSrc zT
                                  !shareBed = if capBedTot > 0.0 then capBed / capBedTot else wWater
                                  !shareSusp = if capSuspTot > 0.0 then capSusp / capSuspTot else wWater
                                  !dqBed = max 0.0 (bedOut * shareBed)
                                  !dqSusp = max 0.0 (suspOut * shareSusp)
                                  !rj = VU.unsafeIndex lakeIdF j
                              if rj >= 0 && VU.unsafeIndex lakeMaskF j /= 0
                                then do
                                  depositSolidVolumeAtFace
                                    law
                                    deltaSpread
                                    fixOutlets
                                    outMask
                                    mesh
                                    faceArea
                                    j
                                    dqBed
                                    bed
                                    (seaLakeDeltaDeposition arena)
                                    bestRef
                                  addVecAt (seaLakeIncomingSusp arena) rj dqSusp
                                else do
                                  reliefAdd (seaBedLoad arena) j dqBed
                                  reliefAdd (seaSuspendedLoad arena) j dqSusp
                              pushLoop (slot + 1)
                pushLoop 0

          faceLoop !k
            | k == rows = pure ()
            | otherwise = do
                let !i = VU.unsafeIndex order k
                loadBedHere <- reliefReadM (seaBedLoad arena) i
                loadSuspHere <- reliefReadM (seaSuspendedLoad arena) i
                let !isActive = VU.unsafeIndex activeMask i /= 0 || loadBedHere > 1.0e-18 || loadSuspHere > 1.0e-18
                if not isActive
                  then faceLoop (k + 1)
                  else do
                    let !r = VU.unsafeIndex lakeIdF i
                    if r >= 0 && VU.unsafeIndex lakeMaskF i /= 0
                      then do
                        if VU.unsafeIndex repFaceF i == i
                          then processLakeRepresentative i r
                          else processNonRepresentativeLakeFace i r
                        faceLoop (k + 1)
                      else do
                        processRiverFace i
                        faceLoop (k + 1)

      faceLoop 0
      readSTRef bestRef

newSedimentArena :: Int -> ST s (SedimentArena s)
newSedimentArena !rows = do
  bedCap <- newReliefMutable rows 0.0
  bedLoad <- newReliefMutable rows 0.0
  bedEro <- newReliefMutable rows 0.0
  bedDep <- newReliefMutable rows 0.0
  suspCap <- newReliefMutable rows 0.0
  suspLoad <- newReliefMutable rows 0.0
  suspEro <- newReliefMutable rows 0.0
  suspDep <- newReliefMutable rows 0.0
  lakeDelta <- newReliefMutable rows 0.0
  lakeSettle <- newReliefMutable rows 0.0
  lakeCarry <- newReliefMutable rows 0.0
  lakeIn <- VUM.replicate rows 0.0
  lakeArea <- VUM.replicate rows 0.0
  lakeSetNum <- VUM.replicate rows 0.0
  lakeCarryWork <- VUM.replicate rows 0.0
  pure
    (SedimentArena
      bedCap
      bedLoad
      bedEro
      bedDep
      suspCap
      suspLoad
      suspEro
      suspDep
      lakeDelta
      lakeSettle
      lakeCarry
      lakeIn
      lakeArea
      lakeSetNum
      lakeCarryWork)

snapshotSedimentState :: SedimentArena s -> ST s SedimentState
snapshotSedimentState !arena = do
  bedCap <- reliefData <$> freezeReliefClone (seaBedCapacity arena)
  bedLoad <- reliefData <$> freezeReliefClone (seaBedLoad arena)
  bedEro <- reliefData <$> freezeReliefClone (seaBedErosion arena)
  bedDep <- reliefData <$> freezeReliefClone (seaBedDeposition arena)
  suspCap <- reliefData <$> freezeReliefClone (seaSuspendedCapacity arena)
  suspLoad <- reliefData <$> freezeReliefClone (seaSuspendedLoad arena)
  suspEro <- reliefData <$> freezeReliefClone (seaSuspendedErosion arena)
  suspDep <- reliefData <$> freezeReliefClone (seaSuspendedDeposition arena)
  lakeDelta <- reliefData <$> freezeReliefClone (seaLakeDeltaDeposition arena)
  lakeSettle <- reliefData <$> freezeReliefClone (seaLakeSettling arena)
  lakeCarry <- reliefData <$> freezeReliefClone (seaLakeSuspendedCarry arena)
  pure
    SedimentState
      { ssBedCapacity = bedCap
      , ssBedLoad = bedLoad
      , ssBedErosion = bedEro
      , ssBedDeposition = bedDep
      , ssSuspendedCapacity = suspCap
      , ssSuspendedLoad = suspLoad
      , ssSuspendedErosion = suspEro
      , ssSuspendedDeposition = suspDep
      , ssLakeDeltaDeposition = lakeDelta
      , ssLakeSettling = lakeSettle
      , ssLakeSuspendedCarry = lakeCarry
      }
