{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Melusine.Algebra.Pure.ChannelDelta
  ( CanonicalChannel (..),
    ChannelWitness (..),
    SomeChannelWitness (..),
    ChannelState,
    ChannelDeltaPayload,
    TypedChannelDelta (..),
    SomeChannelDelta (..),
    Dict (..),
    ChannelConstraintBundle (..),
    DeltaProgram (..),
    ChannelDeltaRegistry (..),
    allChannelConstraints,
    channelWitnessFromId,
    channelIdFromWitness,
    allChannelWitnesses,
    withSomeChannelConstraint,
    channelDeltaRegistry,
    typedChannelWitness,
    typedChannelPayload,
    typedChannelDelta,
    emptyTypedChannelDelta,
    composeTypedChannelDelta,
    applyTypedChannelDelta,
  )
where

import Prelude hiding (Monoid, Semigroup)
import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))
import Melusine.Algebra.Pure.Channel (ChannelId (..))
import Melusine.Algebra.Pure.ClimateVec (ClimateDelta, ClimateVec)
import Melusine.Algebra.Pure.CreaturePopulations (CreaturePopulations, CreaturePopulationsDelta)
import Melusine.Algebra.Pure.EnvironmentTags (EnvironmentTags, TagPatch)
import Melusine.Algebra.Pure.InfluenceField (InfluenceDelta, InfluenceField)
import Melusine.Algebra.Pure.LoreFacts (LoreFacts, LoreFactsDelta)
import Melusine.Algebra.Pure.RuleState (ConstraintDelta, RuleState)
import Moonlight.Algebra (Batch (..), DeltaEndo (..), Magma (..), Monoid (..), Semigroup)
import Moonlight.Category
  ( CoveringFamily (..),
    CoveringConstraints (..),
    Dict (..),
    Exists (..),
  )

data CanonicalChannel
  = LoreChannel
  | ClimateChannel
  | InfluenceChannel
  | PopulationsChannel
  | TagsChannel
  | RulesChannel

data ChannelWitness (channel :: CanonicalChannel) where
  LoreWitness :: ChannelWitness 'LoreChannel
  ClimateWitness :: ChannelWitness 'ClimateChannel
  InfluenceWitness :: ChannelWitness 'InfluenceChannel
  PopulationsWitness :: ChannelWitness 'PopulationsChannel
  TagsWitness :: ChannelWitness 'TagsChannel
  RulesWitness :: ChannelWitness 'RulesChannel

type family ChannelState (channel :: CanonicalChannel) where
  ChannelState 'LoreChannel = LoreFacts
  ChannelState 'ClimateChannel = ClimateVec
  ChannelState 'InfluenceChannel = InfluenceField
  ChannelState 'PopulationsChannel = CreaturePopulations
  ChannelState 'TagsChannel = EnvironmentTags
  ChannelState 'RulesChannel = RuleState

type family ChannelDeltaPayload (channel :: CanonicalChannel) where
  ChannelDeltaPayload 'LoreChannel = LoreFactsDelta
  ChannelDeltaPayload 'ClimateChannel = ClimateDelta
  ChannelDeltaPayload 'InfluenceChannel = InfluenceDelta
  ChannelDeltaPayload 'PopulationsChannel = CreaturePopulationsDelta
  ChannelDeltaPayload 'TagsChannel = TagPatch
  ChannelDeltaPayload 'RulesChannel = ConstraintDelta

data TypedChannelDelta (channel :: CanonicalChannel) where
  LoreTypedDelta :: LoreFactsDelta -> TypedChannelDelta 'LoreChannel
  ClimateTypedDelta :: ClimateDelta -> TypedChannelDelta 'ClimateChannel
  InfluenceTypedDelta :: InfluenceDelta -> TypedChannelDelta 'InfluenceChannel
  PopulationsTypedDelta :: CreaturePopulationsDelta -> TypedChannelDelta 'PopulationsChannel
  TagsTypedDelta :: TagPatch -> TypedChannelDelta 'TagsChannel
  RulesTypedDelta :: ConstraintDelta -> TypedChannelDelta 'RulesChannel

data SomeChannelDelta where
  SomeChannelDelta :: TypedChannelDelta channel -> SomeChannelDelta

data SomeChannelWitness where
  SomeChannelWitness :: ChannelWitness channel -> SomeChannelWitness

data ChannelConstraintBundle (constraint :: CanonicalChannel -> Constraint) = ChannelConstraintBundle
  { loreConstraint :: Dict (constraint 'LoreChannel),
    climateConstraint :: Dict (constraint 'ClimateChannel),
    influenceConstraint :: Dict (constraint 'InfluenceChannel),
    populationsConstraint :: Dict (constraint 'PopulationsChannel),
    tagsConstraint :: Dict (constraint 'TagsChannel),
    rulesConstraint :: Dict (constraint 'RulesChannel)
  }

newtype DeltaProgram = DeltaProgram [SomeChannelDelta]
  deriving (Magma, Semigroup, Monoid) via (Batch SomeChannelDelta)

data ChannelDeltaRegistry (channel :: CanonicalChannel) = ChannelDeltaRegistry
  { registryIdentityDelta :: ChannelDeltaPayload channel,
    registryComposeDelta :: ChannelDeltaPayload channel -> ChannelDeltaPayload channel -> ChannelDeltaPayload channel,
    registryApplyDelta :: ChannelDeltaPayload channel -> ChannelState channel -> ChannelState channel
  }

instance Show DeltaProgram where
  show (DeltaProgram deltas) = "DeltaProgram " ++ show (length deltas)

allChannelConstraints ::
  forall constraint.
  ( constraint 'LoreChannel,
    constraint 'ClimateChannel,
    constraint 'InfluenceChannel,
    constraint 'PopulationsChannel,
    constraint 'TagsChannel,
    constraint 'RulesChannel
  ) =>
  ChannelConstraintBundle constraint
allChannelConstraints =
  ChannelConstraintBundle
    { loreConstraint = Dict,
      climateConstraint = Dict,
      influenceConstraint = Dict,
      populationsConstraint = Dict,
      tagsConstraint = Dict,
      rulesConstraint = Dict
    }

channelWitnessFromId :: ChannelId -> SomeChannelWitness
channelWitnessFromId channelKey =
  case channelKey of
    LoreFactsCh -> SomeChannelWitness LoreWitness
    ClimateVecCh -> SomeChannelWitness ClimateWitness
    InfluenceFieldCh -> SomeChannelWitness InfluenceWitness
    CreaturePopulationsCh -> SomeChannelWitness PopulationsWitness
    EnvironmentTagsCh -> SomeChannelWitness TagsWitness
    RuleStateCh -> SomeChannelWitness RulesWitness

channelIdFromWitness :: ChannelWitness channel -> ChannelId
channelIdFromWitness witness =
  case witness of
    LoreWitness -> LoreFactsCh
    ClimateWitness -> ClimateVecCh
    InfluenceWitness -> InfluenceFieldCh
    PopulationsWitness -> CreaturePopulationsCh
    TagsWitness -> EnvironmentTagsCh
    RulesWitness -> RuleStateCh

allChannelWitnesses :: [SomeChannelWitness]
allChannelWitnesses = map channelWitnessFromId [minBound .. maxBound]

withSomeChannelConstraint ::
  forall constraint result.
  ChannelConstraintBundle constraint ->
  SomeChannelWitness ->
  (forall channel. constraint channel => Proxy channel -> result) ->
  result
withSomeChannelConstraint constraintBundle someWitness continuation =
  case someWitness of
    SomeChannelWitness witness ->
      case witness of
        LoreWitness ->
          case loreConstraint constraintBundle of
            Dict -> continuation (Proxy @'LoreChannel)
        ClimateWitness ->
          case climateConstraint constraintBundle of
            Dict -> continuation (Proxy @'ClimateChannel)
        InfluenceWitness ->
          case influenceConstraint constraintBundle of
            Dict -> continuation (Proxy @'InfluenceChannel)
        PopulationsWitness ->
          case populationsConstraint constraintBundle of
            Dict -> continuation (Proxy @'PopulationsChannel)
        TagsWitness ->
          case tagsConstraint constraintBundle of
            Dict -> continuation (Proxy @'TagsChannel)
        RulesWitness ->
          case rulesConstraint constraintBundle of
            Dict -> continuation (Proxy @'RulesChannel)

mkRegistry ::
  DeltaEndo (ChannelDeltaPayload channel) (ChannelState channel) =>
  ChannelDeltaRegistry channel
mkRegistry =
  ChannelDeltaRegistry
    { registryIdentityDelta = identityDelta,
      registryComposeDelta = composeDelta,
      registryApplyDelta = applyDelta
    }

channelDeltaRegistry :: ChannelWitness channel -> ChannelDeltaRegistry channel
channelDeltaRegistry witness =
  case witness of
    LoreWitness -> mkRegistry
    ClimateWitness -> mkRegistry
    InfluenceWitness -> mkRegistry
    PopulationsWitness -> mkRegistry
    TagsWitness -> mkRegistry
    RulesWitness -> mkRegistry

typedChannelWitness :: TypedChannelDelta channel -> ChannelWitness channel
typedChannelWitness typedDelta =
  case typedDelta of
    LoreTypedDelta _ -> LoreWitness
    ClimateTypedDelta _ -> ClimateWitness
    InfluenceTypedDelta _ -> InfluenceWitness
    PopulationsTypedDelta _ -> PopulationsWitness
    TagsTypedDelta _ -> TagsWitness
    RulesTypedDelta _ -> RulesWitness

typedChannelPayload :: TypedChannelDelta channel -> ChannelDeltaPayload channel
typedChannelPayload typedDelta =
  case typedDelta of
    LoreTypedDelta deltaValue -> deltaValue
    ClimateTypedDelta deltaValue -> deltaValue
    InfluenceTypedDelta deltaValue -> deltaValue
    PopulationsTypedDelta deltaValue -> deltaValue
    TagsTypedDelta deltaValue -> deltaValue
    RulesTypedDelta deltaValue -> deltaValue

typedChannelDelta :: ChannelWitness channel -> ChannelDeltaPayload channel -> TypedChannelDelta channel
typedChannelDelta witness deltaValue =
  case witness of
    LoreWitness -> LoreTypedDelta deltaValue
    ClimateWitness -> ClimateTypedDelta deltaValue
    InfluenceWitness -> InfluenceTypedDelta deltaValue
    PopulationsWitness -> PopulationsTypedDelta deltaValue
    TagsWitness -> TagsTypedDelta deltaValue
    RulesWitness -> RulesTypedDelta deltaValue

emptyTypedChannelDelta :: ChannelWitness channel -> TypedChannelDelta channel
emptyTypedChannelDelta witness =
  typedChannelDelta witness (registryIdentityDelta (channelDeltaRegistry witness))

composeTypedChannelDelta :: TypedChannelDelta channel -> TypedChannelDelta channel -> TypedChannelDelta channel
composeTypedChannelDelta left right =
  typedChannelDelta
    witness
    (registryComposeDelta registry (typedChannelPayload left) (typedChannelPayload right))
  where
    witness = typedChannelWitness left
    registry = channelDeltaRegistry witness

applyTypedChannelDelta :: TypedChannelDelta channel -> ChannelState channel -> ChannelState channel
applyTypedChannelDelta typedDelta stateValue =
  registryApplyDelta registry (typedChannelPayload typedDelta) stateValue
  where
    registry = channelDeltaRegistry (typedChannelWitness typedDelta)

instance CoveringFamily ChannelWitness where
  allMembers =
    [ Exists LoreWitness,
      Exists ClimateWitness,
      Exists InfluenceWitness,
      Exists PopulationsWitness,
      Exists TagsWitness,
      Exists RulesWitness
    ]

instance
  ( constraint 'LoreChannel,
    constraint 'ClimateChannel,
    constraint 'InfluenceChannel,
    constraint 'PopulationsChannel,
    constraint 'TagsChannel,
    constraint 'RulesChannel
  ) =>
  CoveringConstraints ChannelWitness constraint
  where
  constraintDict LoreWitness = Dict
  constraintDict ClimateWitness = Dict
  constraintDict InfluenceWitness = Dict
  constraintDict PopulationsWitness = Dict
  constraintDict TagsWitness = Dict
  constraintDict RulesWitness = Dict
