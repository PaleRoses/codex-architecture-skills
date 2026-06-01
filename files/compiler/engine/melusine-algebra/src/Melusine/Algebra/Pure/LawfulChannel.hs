{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Melusine.Algebra.Pure.LawfulChannel
  ( LawFamily (..),
    allLawFamilies,
    LawfulChannel,
    LawFlags,
    mkLawFlags,
    lawFamilies,
    hasLawFlag,
    ChannelLawDescriptorPayload (..),
    ChannelLawFamilyCoverage,
    ChannelLawContract,
    HasLaw,
    RequiresLaw,
    reflectLawFlags,
    channelLawDescriptorRegistry,
    channelLawFlagsFromWitness,
    SemiringLawCarrier,
    SemiringLawProjection (..),
    allLawfulChannelConstraints,
    allChannelLawContractConstraints,
  )
where

import Data.Proxy (Proxy (..))
import Data.Kind (Type)
import qualified Data.Set as Set
import Melusine.Algebra.Pure.ChannelDescriptor
  ( ChannelDescriptor (..),
    descriptorAt,
    tabulateChannelDescriptorsWithConstraint,
  )
import Melusine.Algebra.Pure.ChannelDelta
  ( CanonicalChannel (..),
    ChannelConstraintBundle,
    ChannelState,
    ChannelWitness,
    allChannelConstraints,
  )
import Melusine.Algebra.Pure.ChannelVec
  ( ChannelVec )
import Melusine.Algebra.Pure.EqPolicy (EqPolicy)
import Melusine.Algebra.Pure.InfluenceField (InfluenceSample, sampleInfluenceField)
import Moonlight.Algebra (Semiring)

data LawFamily
  = ActionLaw
  | InvertibleActionLaw
  | SemilatticeLaw
  | GroupLaw
  | SemiringLaw
  | BooleanLaw
  | HeytingLaw
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

allLawFamilies :: [LawFamily]
allLawFamilies = [minBound .. maxBound]

class LawfulChannel (channel :: CanonicalChannel)

instance LawfulChannel 'LoreChannel

instance LawfulChannel 'ClimateChannel

instance LawfulChannel 'InfluenceChannel

instance LawfulChannel 'PopulationsChannel

instance LawfulChannel 'TagsChannel

instance LawfulChannel 'RulesChannel

type family ChannelLawSet (channel :: CanonicalChannel) :: [LawFamily] where
  ChannelLawSet 'LoreChannel = '[ 'ActionLaw, 'SemilatticeLaw]
  ChannelLawSet 'ClimateChannel = '[ 'ActionLaw, 'InvertibleActionLaw, 'GroupLaw]
  ChannelLawSet 'InfluenceChannel = '[ 'ActionLaw, 'SemiringLaw]
  ChannelLawSet 'PopulationsChannel = '[ 'ActionLaw, 'InvertibleActionLaw, 'GroupLaw]
  ChannelLawSet 'TagsChannel = '[ 'ActionLaw, 'BooleanLaw]
  ChannelLawSet 'RulesChannel = '[ 'ActionLaw, 'HeytingLaw]

type family ElemLaw (law :: LawFamily) (lawSet :: [LawFamily]) :: Bool where
  ElemLaw _ '[] = 'False
  ElemLaw law (law ': remaining) = 'True
  ElemLaw law (_ ': remaining) = ElemLaw law remaining

type family HasLaw (channel :: CanonicalChannel) (law :: LawFamily) :: Bool where
  HasLaw channel law = ElemLaw law (ChannelLawSet channel)

type RequiresLaw channel law = (HasLaw channel law ~ 'True)

newtype LawFlags = LawFlags
  { unLawFlags :: Set.Set LawFamily
  }
  deriving stock (Eq, Show, Read)

mkLawFlags :: [LawFamily] -> LawFlags
mkLawFlags = LawFlags . Set.fromList

lawFamilies :: LawFlags -> [LawFamily]
lawFamilies (LawFlags flags) = Set.toAscList flags

hasLawFlag :: LawFamily -> LawFlags -> Bool
hasLawFlag lawFamily (LawFlags flags) = Set.member lawFamily flags

insertLawFlag :: LawFamily -> LawFlags -> LawFlags
insertLawFlag lawFamily (LawFlags flags) = LawFlags (Set.insert lawFamily flags)

class KnownLawFamily (law :: LawFamily) where
  reflectLawFamily :: Proxy law -> LawFamily

instance KnownLawFamily 'ActionLaw where
  reflectLawFamily _ = ActionLaw

instance KnownLawFamily 'InvertibleActionLaw where
  reflectLawFamily _ = InvertibleActionLaw

instance KnownLawFamily 'SemilatticeLaw where
  reflectLawFamily _ = SemilatticeLaw

instance KnownLawFamily 'GroupLaw where
  reflectLawFamily _ = GroupLaw

instance KnownLawFamily 'SemiringLaw where
  reflectLawFamily _ = SemiringLaw

instance KnownLawFamily 'BooleanLaw where
  reflectLawFamily _ = BooleanLaw

instance KnownLawFamily 'HeytingLaw where
  reflectLawFamily _ = HeytingLaw

data ChannelLawDescriptorPayload (channel :: CanonicalChannel) = ChannelLawDescriptorPayload
  { channelDescriptorLawFlags :: LawFlags
  }

class ReflectLawSet (lawSet :: [LawFamily]) where
  reflectLawSet :: Proxy lawSet -> LawFlags

instance ReflectLawSet '[] where
  reflectLawSet _ = mkLawFlags []

instance
  ( KnownLawFamily law,
    ReflectLawSet remaining
  ) =>
  ReflectLawSet (law ': remaining)
  where
  reflectLawSet _ =
    insertLawFlag
      (reflectLawFamily (Proxy @law))
      (reflectLawSet (Proxy @remaining))

type ChannelLawFamilyCoverage (channel :: CanonicalChannel) =
  ReflectLawSet (ChannelLawSet channel)

class
  ( LawfulChannel channel,
    ChannelLawFamilyCoverage channel
  ) =>
  ChannelLawContract (channel :: CanonicalChannel)

instance
  ( LawfulChannel channel,
    ChannelLawFamilyCoverage channel
  ) =>
  ChannelLawContract channel

reflectLawFlags ::
  forall channel proxy.
  ChannelLawContract channel =>
  proxy channel ->
  LawFlags
reflectLawFlags _ =
  reflectLawSet (Proxy @(ChannelLawSet channel))

channelLawDescriptorRegistry :: ChannelVec (ChannelDescriptor ChannelLawDescriptorPayload)
channelLawDescriptorRegistry =
  tabulateChannelDescriptorsWithConstraint
    allChannelLawContractConstraints
    (\(_ :: ChannelWitness channel) ->
       ChannelLawDescriptorPayload (reflectLawFlags (Proxy @channel)))

channelLawFlagsFromWitness ::
  ChannelWitness channel ->
  LawFlags
channelLawFlagsFromWitness witness =
  channelDescriptorLawFlags (descriptorPayload (descriptorAt witness channelLawDescriptorRegistry))

type family SemiringLawCarrier (channel :: CanonicalChannel) = (carrier :: Type) | carrier -> channel where
  SemiringLawCarrier 'InfluenceChannel = InfluenceSample

class
  ( RequiresLaw channel 'SemiringLaw,
    EqPolicy (SemiringLawCarrier channel),
    Semiring (SemiringLawCarrier channel)
  ) =>
  SemiringLawProjection (channel :: CanonicalChannel)
  where
  projectSemiringLawCarrier :: ChannelState channel -> SemiringLawCarrier channel

instance SemiringLawProjection 'InfluenceChannel where
  projectSemiringLawCarrier = sampleInfluenceField

allLawfulChannelConstraints :: ChannelConstraintBundle LawfulChannel
allLawfulChannelConstraints = allChannelConstraints @LawfulChannel

allChannelLawContractConstraints :: ChannelConstraintBundle ChannelLawContract
allChannelLawContractConstraints = allChannelConstraints @ChannelLawContract
