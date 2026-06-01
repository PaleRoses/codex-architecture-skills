{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.World.Growth.Solver.Solstice.Engine.Types
  ( MFloatVec
  , MorphogenicError(..)
  , MorphogenicState(..)
  , MorphogenicStepStats(..)
  , zeroMorphogenicStepStats
  , MorphogenicProgram(..)
  , MorphogenicEngineProfiling(..)
  , ExplicitArena(..)
  , ImplicitArena(..)
  ) where

import Data.Kind (Type)
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as VU

import Melusine.World.Growth.Internal.Channels (ScalarField)
import Melusine.World.Growth.Solver.Common.Relief (MReliefBlock)
import Melusine.World.Growth.Solver.Common.Util (MFloatVec)
import Melusine.World.Growth.Solver.Solstice.Drainage (DrainageError)
import Melusine.World.Growth.Solver.Solstice.Hydrology.Arena (HydrologyArena, HydrologyArenaError, HydrologyState)
import Melusine.World.Growth.Solver.Solstice.Laws.Relief (MorphogenicAdapt)
import Melusine.World.Growth.Solver.Solstice.Sediment.Arena (SedimentArena, SedimentState)
import Melusine.World.Growth.Solver.Solstice.WaterSurface (WaterSurfaceError)
import Moonlight.Analysis.Mesh.Scalar (MGArena, MGLevelOp, ScalarLevelOp)

type MorphogenicError :: Type
data MorphogenicError
  = MorphogenicInvalidInput !String
  | MorphogenicInvalidLaw !String
  | MorphogenicDrainage !DrainageError
  | MorphogenicHydrology !HydrologyArenaError
  | MorphogenicWater !WaterSurfaceError
  | MorphogenicCompose !String
  deriving stock (Eq, Show)

type MorphogenicState :: Type
data MorphogenicState = MorphogenicState
  { mgsRows                :: !Int
  , mgsIterations          :: !Int
  , mgsPuissance           :: !ScalarField
  , mgsPuissanceSurface    :: !ScalarField
  , mgsHydrology           :: !HydrologyState
  , mgsSediment            :: !SedimentState
  , mgsDirtyFaces          :: !(VU.Vector Int)
  , mgsDirtyBasins         :: !(VU.Vector Int)
  , mgsActiveFaces         :: !(VU.Vector Int)
  , mgsFluvialCutMask      :: !(VU.Vector Word8)
  , mgsFluvialMaxDelta     :: !Double
  , mgsBankMaxDelta        :: !Double
  , mgsSedimentMaxDelta    :: !Double
  , mgsDiffusionMaxDelta   :: !Double
  , mgsConvexityMaxDelta   :: !Double
  , mgsDiffusionResidual   :: !Float
  , mgsDiffusionIterations :: !Int
  }

type MorphogenicStepStats :: Type
data MorphogenicStepStats = MorphogenicStepStats
  { mssFluvialDelta   :: !Double
  , mssFluvialCutMask :: !(VU.Vector Word8)
  , mssBankDelta      :: !Double
  , mssSedimentDelta  :: !Double
  , mssDiffusionDelta :: !Double
  , mssConvexityDelta :: !Double
  , mssDiffResidual   :: !Float
  , mssDiffIters      :: !Int
  }

zeroMorphogenicStepStats :: MorphogenicStepStats
zeroMorphogenicStepStats =
  MorphogenicStepStats
    { mssFluvialDelta = 0.0
    , mssFluvialCutMask = VU.empty
    , mssBankDelta = 0.0
    , mssSedimentDelta = 0.0
    , mssDiffusionDelta = 0.0
    , mssConvexityDelta = 0.0
    , mssDiffResidual = 0.0
    , mssDiffIters = 0
    }

type MorphogenicProgram :: Type -> Type
data MorphogenicProgram s = MorphogenicProgram
  { mpgHydrologyArena :: !(HydrologyArena s)
  , mpgSedimentArena  :: !(SedimentArena s)
  , mpgExplicitArena  :: !(ExplicitArena s)
  , mpgImplicitArena  :: !(ImplicitArena s)
  , mpgPuissanceCur   :: !(MReliefBlock s)
  , mpgPuissanceNext  :: !(MReliefBlock s)
  , mpgHydrology      :: !HydrologyState
  , mpgAdapt          :: !MorphogenicAdapt
  , mpgStepStats      :: !MorphogenicStepStats
  , mpgIterationsDone :: !Int
  }

type MorphogenicEngineProfiling :: Type
data MorphogenicEngineProfiling = MorphogenicEngineProfiling
  { mepInitialHydrologySeconds :: !Double
  , mepFrontierSeconds :: !Double
  , mepHydrologyAdvanceSeconds :: !Double
  , mepHydrologyProjectSeconds :: !Double
  , mepActiveMaskSeconds :: !Double
  , mepFluvialSeconds :: !Double
  , mepBankSeconds :: !Double
  , mepSedimentSeconds :: !Double
  , mepDiffusionSeconds :: !Double
  , mepConvexitySeconds :: !Double
  , mepFinishSeconds :: !Double
  , mepTotalSeconds :: !Double
  , mepIterations :: !Int
  , mepImplicitDiffusionCalls :: !Int
  }
  deriving stock (Eq, Show)

type ExplicitArena :: Type -> Type
data ExplicitArena s = ExplicitArena
  { epaPuissanceCur  :: !(MReliefBlock s)
  , epaPuissanceNext :: !(MReliefBlock s)
  , epaPuissanceTmp  :: !(MReliefBlock s)
  , epaScratch       :: !(MReliefBlock s)
  }

type ImplicitArena :: Type -> Type
data ImplicitArena s = ImplicitArena
  { ipaPairWeights :: !(MFloatVec s)
  , ipaFineOp      :: !ScalarLevelOp
  , ipaMgAnchor    :: !ScalarLevelOp
  , ipaMgOp        :: !MGLevelOp
  , ipaMGArena     :: !(MGArena s)
  , ipaX           :: !(MFloatVec s)
  , ipaB           :: !(MFloatVec s)
  , ipaR           :: !(MFloatVec s)
  , ipaP           :: !(MFloatVec s)
  , ipaZ           :: !(MFloatVec s)
  , ipaAp          :: !(MFloatVec s)
  }
