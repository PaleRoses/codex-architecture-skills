module Melusine.World.Growth.Solver.PrimordialField.ImplicitOwnershipSpec
  ( tests
  ) where

import Control.Monad.ST (runST)
import Data.Maybe (isNothing)
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Data.Vector qualified as VB
import Data.Vector.Unboxed qualified as VU
import Melusine.World.Growth.Core.Types
  ( axisCount
  )
import Melusine.World.Growth.Solver.PrimordialField.Calibration.Types
  ( CompiledSolverCalibration(..)
  )
import Melusine.World.Growth.Solver.PrimordialField.Calibration.Compile
  ( compileSolverCalibration
  )
import Melusine.World.Growth.Solver.PrimordialField.Calibration.Defaults
  ( defaultSolverCalibration
  )
import Melusine.World.Growth.Solver.PrimordialField.Engine
  ( runWorldSolver
  )
import Melusine.World.Growth.Solver.PrimordialField.Calibration.Laws
  ( buildPressureDiffusionMatrix
  )
import Melusine.World.Growth.Solver.PrimordialField.Core.Types
  ( PressureSchedule(..)
  , PressureScheduleSwitch(..)
  , PressureScheduleTrigger(..)
  , PressureStepMethod(..)
  , SolverTuning(..)
  , WorldContext(..)
  , WorldState(..)
  )
import Melusine.World.Growth.Solver.PrimordialField.Pressure.Block
  ( Pressure8(..)
  , pressureData
  , pressureGenerate
  , pressureRead8
  , pressureRead8M
  , pressureRows
  , maxAbs8
  , sub8
  , thawPressure
  )
import Melusine.World.Growth.Solver.PrimordialField.Pressure.CoupledStep
  ( CoupledExpr(..)
  , CoupledField(..)
  , CoupledSolveStats(..)
  , coupledArenaDeltaWithDirty
  , materializeCoupledArena
  , newCoupledArena
  , readCoupledFace8
  , seedCoupledArena
  , snapshotCoupledArena
  )
import Melusine.World.Growth.Solver.PrimordialField.Core.LawTypes
  ( PressureLaw(plClipHi, plClipLo, plDt)
  , VolumeMBOParams(vpPressurePull)
  )
import Melusine.World.Growth.Solver.PrimordialField.Pressure.Projection
  ( PressureReader(..)
  , modalPressureReader
  )
import Melusine.World.Growth.Solver.PrimordialField.Report
  ( PrimordialImplicitStepReport(..)
  , PrimordialSolverReport(..)
  )
import Melusine.World.Growth.Solver.PrimordialField.Core.Schedule
  ( AdaptParams(..)
  )
import Melusine.World.Growth.TestSupport.PrimordialField
  ( tinyWorld
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "implicit-pressure-ownership"
    [ testCase "modal pressure reader matches dense backprojection" modalPressureReaderSpec
    , testCase "modal dirty delta matches dense materialization" modalArenaDeltaSpec
    , testCase "implicit checkpoints materialize dense pressure and final state clears Anderson history" implicitCheckpointSpec
    , testCase "solver pressure state remains within signed unit climate bounds" signedUnitPressureInvariantSpec
    ]

modalPressureReaderSpec :: IO ()
modalPressureReaderSpec = do
  let compiled = compileSolverCalibration defaultSolverCalibration
      faces = VU.fromList [0, 1, 2]
      rows = 3
      press =
        pressureGenerate rows $ \faceIx ->
          Pressure8
            (0.10 * fromIntegral (faceIx + 1))
            (0.05 * fromIntegral faceIx)
            (0.20 * fromIntegral (faceIx + 1))
            (-0.05 * fromIntegral faceIx)
            0.03
            (-0.02)
            (0.04 * fromIntegral faceIx)
            0.01
      measured =
        runST $ do
          mutablePress <- thawPressure press
          arena <- newCoupledArena rows (coupledFieldFor compiled)
          seedCoupledArena (pressureRead8M mutablePress) arena rows faces
          dense <- materializeCoupledArena arena rows faces
          let reader = modalPressureReader (readCoupledFace8 arena rows)
          traverse
            (\faceIx ->
                do
                  readBack <- prReadFace8 reader faceIx
                  pure (maxAbs8 (sub8 readBack (pressureRead8 dense faceIx)))
            )
            [0, 1, 2]
  assertBool "coupled reader should match dense backprojection" (all (< 1.0e-5) measured)

modalArenaDeltaSpec :: IO ()
modalArenaDeltaSpec = do
  let compiled = compileSolverCalibration defaultSolverCalibration
      faces = VU.fromList [0, 1, 2]
      rows = 3
      oldPress =
        pressureGenerate rows $ \faceIx ->
          Pressure8
            (0.20 + 0.03 * fromIntegral faceIx)
            0.10
            (0.05 * fromIntegral (faceIx + 1))
            0.02
            (-0.04)
            0.03
            0.01
            (-0.02)
      newPress =
        pressureGenerate rows $ \faceIx ->
          Pressure8
            (0.20 + 0.03 * fromIntegral faceIx + if faceIx == 1 then 0.12 else 0.01)
            0.10
            (0.05 * fromIntegral (faceIx + 1))
            (if faceIx == 2 then 0.18 else 0.02)
            (-0.04)
            0.03
            0.01
            (-0.02)
      (deltaValue, dirtyFaces) =
        runST $ do
          mutableOld <- thawPressure oldPress
          mutableNew <- thawPressure newPress
          arena <- newCoupledArena rows (coupledFieldFor compiled)
          seedCoupledArena (pressureRead8M mutableOld) arena rows faces
          snapshotCoupledArena arena
          oldDense <- materializeCoupledArena arena rows faces
          seedCoupledArena (pressureRead8M mutableNew) arena rows faces
          newDense <- materializeCoupledArena arena rows faces
          dirtyRef <- newSTRef []
          deltaF <-
            coupledArenaDeltaWithDirty
              arena
              rows
              faces
              0.08
              (\faceIx -> modifySTRef' dirtyRef (\acc -> acc ++ [faceIx]))
          dirty <- readSTRef dirtyRef
          let expectedDeltas =
                fmap
                  (\faceIx -> maxAbs8 (sub8 (pressureRead8 newDense faceIx) (pressureRead8 oldDense faceIx)))
                  [0, 1, 2]
              expectedDelta = maximum expectedDeltas
              expectedDirty =
                fmap fst
                  (filter (\(_, deltaFace) -> deltaFace > 0.08) (zip [0, 1, 2] expectedDeltas))
          pure ((abs (deltaF - expectedDelta), expectedDirty), dirty)
  assertBool "coupled delta should match dense delta" (fst deltaValue < 1.0e-5)
  dirtyFaces @?= snd deltaValue

implicitCheckpointSpec :: IO ()
implicitCheckpointSpec = do
  let (ctx0, st0) = tinyWorld
      sentinelContrast = 97.0
      sentinelDiffusionMul = 13.0
      sentinelExotic = 11.0
      sentinelDiffusion = VU.replicate axisCount 7.0
      tuning0 = wcTuning ctx0
      ctx =
        ctx0
          { wcIterations = 9
          , wcTuning =
              tuning0
                { stPressureSchedule =
                    PhasedPressureSchedule
                      ExplicitAndersonStep
                      (VB.singleton (PressureScheduleSwitch (PressureSwitchAfterIteration 2) ImplicitPCGStep))
                }
          , wcAdaptFeedback =
              \press adapt ->
                if pressureRows press == 4 && VU.length (pressureData press) == 32
                  then
                    adapt
                      { apPressureDiffusion = sentinelDiffusion
                      , apContrastSharpness = sentinelContrast
                      , apDiffusionMultiplier = sentinelDiffusionMul
                      , apExoticBoost = sentinelExotic
                      }
                  else adapt
          }
      (finalState, solverReport) = runWorldSolver ctx st0
  assertReportInvariants solverReport
  pressureRows (wsPressures finalState) @?= 4
  VU.length (pressureData (wsPressures finalState)) @?= 32
  apPressureDiffusion (wsAdapt finalState) @?= sentinelDiffusion
  apContrastSharpness (wsAdapt finalState) @?= sentinelContrast
  apDiffusionMultiplier (wsAdapt finalState) @?= sentinelDiffusionMul
  apExoticBoost (wsAdapt finalState) @?= sentinelExotic
  assertBool "implicit completion should clear previous Anderson output" (isNothing (wsPrevPressureOut finalState))
  assertBool "implicit completion should clear previous Anderson residual" (isNothing (wsPrevResidual finalState))

signedUnitPressureInvariantSpec :: IO ()
signedUnitPressureInvariantSpec = do
  let (ctx, st0) = tinyWorld
      (finalState, solverReport) = runWorldSolver ctx st0
      withinSignedUnit :: Float -> Bool
      withinSignedUnit value = abs (realToFrac value :: Double) <= 1.0 + 1.0e-6
  assertReportInvariants solverReport
  assertBool
    "all solver pressure coordinates should stay inside [-1,1]"
    (all withinSignedUnit (VU.toList (pressureData (wsPressures finalState))))

assertReportInvariants :: PrimordialSolverReport -> IO ()
assertReportInvariants report =
  let implicitSteps = psrImplicitSteps report
      maxModalResidual =
        VB.foldl'
          (\acc stepValue -> max acc (cssMaxResidual (pisrCoupledSolve stepValue)))
          0.0
          implicitSteps
      maxModalIterations =
        VB.foldl'
          (\acc stepValue -> max acc (cssMaxIterations (pisrCoupledSolve stepValue)))
          0
          implicitSteps
  in do
    psrHeavySteps report @?= psrMultigridReused report + psrMultigridRebuilt report
    psrHeavySteps report @?= psrCoupledRankReused report + psrCoupledArenaReallocated report
    psrMaxModalResidual report @?= maxModalResidual
    psrMaxModalIterations report @?= maxModalIterations

coupledFieldFor :: CompiledSolverCalibration -> CoupledField s
coupledFieldFor compiled =
  let law = cscPressureLaw compiled
  in CoupledField
      { cfDt = plDt law
      , cfImplicitPull = VU.zipWith (+) (cscSourcePull compiled) (vpPressurePull (cscPhaseLaw compiled))
      , cfDiffusionMat = buildPressureDiffusionMatrix law (apPressureDiffusion (cscInitialAdapt compiled))
      , cfKnown = CEZero
      , cfClipLo = realToFrac (plClipLo law)
      , cfClipHi = realToFrac (plClipHi law)
      , cfRank = 6
      }
