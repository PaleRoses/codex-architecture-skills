{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module Melusine.Algebra.Pure.ChannelVec
  ( ChannelVec (..),
    tabulateChannelVec,
    tabulateChannelVecA,
    tabulateChannelVecWithConstraint,
    mapChannelVec,
    mapChannelVecWithWitness,
    zipChannelVecWithWitness,
    traverseChannelVecWithWitness,
    foldMapChannelVecWithWitness,
    foldMapZipChannelVecWithWitness,
    indexChannelVec,
    replaceChannelVec,
  )
where

import Data.Functor.Const (Const (..), getConst)
import Data.Kind (Type)
import Melusine.Algebra.Pure.ChannelDelta
  ( CanonicalChannel (..),
    ChannelConstraintBundle (..),
    ChannelWitness (..),
    Dict (..),
  )

data ChannelVec (f :: CanonicalChannel -> Type) = ChannelVec
  { channelLore :: f 'LoreChannel,
    channelClimate :: f 'ClimateChannel,
    channelInfluence :: f 'InfluenceChannel,
    channelPopulations :: f 'PopulationsChannel,
    channelTags :: f 'TagsChannel,
    channelRules :: f 'RulesChannel
  }

deriving stock instance
  ( Eq (f 'LoreChannel),
    Eq (f 'ClimateChannel),
    Eq (f 'InfluenceChannel),
    Eq (f 'PopulationsChannel),
    Eq (f 'TagsChannel),
    Eq (f 'RulesChannel)
  ) =>
  Eq (ChannelVec f)

deriving stock instance
  ( Show (f 'LoreChannel),
    Show (f 'ClimateChannel),
    Show (f 'InfluenceChannel),
    Show (f 'PopulationsChannel),
    Show (f 'TagsChannel),
    Show (f 'RulesChannel)
  ) =>
  Show (ChannelVec f)

deriving stock instance
  ( Read (f 'LoreChannel),
    Read (f 'ClimateChannel),
    Read (f 'InfluenceChannel),
    Read (f 'PopulationsChannel),
    Read (f 'TagsChannel),
    Read (f 'RulesChannel)
  ) =>
  Read (ChannelVec f)

tabulateChannelVec ::
  (forall channel. ChannelWitness channel -> f channel) ->
  ChannelVec f
tabulateChannelVec buildValue =
  ChannelVec
    { channelLore = buildValue LoreWitness,
      channelClimate = buildValue ClimateWitness,
      channelInfluence = buildValue InfluenceWitness,
      channelPopulations = buildValue PopulationsWitness,
      channelTags = buildValue TagsWitness,
      channelRules = buildValue RulesWitness
    }

tabulateChannelVecA ::
  Applicative applicative =>
  (forall channel. ChannelWitness channel -> applicative (f channel)) ->
  applicative (ChannelVec f)
tabulateChannelVecA buildValue =
  ChannelVec
    <$> buildValue LoreWitness
    <*> buildValue ClimateWitness
    <*> buildValue InfluenceWitness
    <*> buildValue PopulationsWitness
    <*> buildValue TagsWitness
    <*> buildValue RulesWitness

tabulateChannelVecWithConstraint ::
  ChannelConstraintBundle constraint ->
  (forall channel. constraint channel => ChannelWitness channel -> f channel) ->
  ChannelVec f
tabulateChannelVecWithConstraint constraintBundle buildValue =
  ChannelVec
    { channelLore =
        case loreConstraint constraintBundle of
          Dict -> buildValue LoreWitness,
      channelClimate =
        case climateConstraint constraintBundle of
          Dict -> buildValue ClimateWitness,
      channelInfluence =
        case influenceConstraint constraintBundle of
          Dict -> buildValue InfluenceWitness,
      channelPopulations =
        case populationsConstraint constraintBundle of
          Dict -> buildValue PopulationsWitness,
      channelTags =
        case tagsConstraint constraintBundle of
          Dict -> buildValue TagsWitness,
      channelRules =
        case rulesConstraint constraintBundle of
          Dict -> buildValue RulesWitness
    }

mapChannelVec ::
  (forall channel. f channel -> g channel) ->
  ChannelVec f ->
  ChannelVec g
mapChannelVec mapValue = mapChannelVecWithWitness (\_ -> mapValue)

mapChannelVecWithWitness ::
  (forall channel. ChannelWitness channel -> f channel -> g channel) ->
  ChannelVec f ->
  ChannelVec g
mapChannelVecWithWitness mapValue channelVecValue =
  tabulateChannelVec
    (\witness -> mapValue witness (indexChannelVec witness channelVecValue))

zipChannelVecWithWitness ::
  (forall channel. ChannelWitness channel -> f channel -> g channel -> h channel) ->
  ChannelVec f ->
  ChannelVec g ->
  ChannelVec h
zipChannelVecWithWitness zipValue left right =
  tabulateChannelVec
    ( \witness ->
        zipValue
          witness
          (indexChannelVec witness left)
          (indexChannelVec witness right)
    )

traverseChannelVecWithWitness ::
  Applicative f =>
  (forall channel. ChannelWitness channel -> g channel -> f (h channel)) ->
  ChannelVec g ->
  f (ChannelVec h)
traverseChannelVecWithWitness traverseValue channelVecValue =
  ChannelVec
    <$> traverseValue LoreWitness (channelLore channelVecValue)
    <*> traverseValue ClimateWitness (channelClimate channelVecValue)
    <*> traverseValue InfluenceWitness (channelInfluence channelVecValue)
    <*> traverseValue PopulationsWitness (channelPopulations channelVecValue)
    <*> traverseValue TagsWitness (channelTags channelVecValue)
    <*> traverseValue RulesWitness (channelRules channelVecValue)

foldMapChannelVecWithWitness ::
  Monoid m =>
  (forall channel. ChannelWitness channel -> f channel -> m) ->
  ChannelVec f ->
  m
foldMapChannelVecWithWitness foldValue channelVecValue =
  getConst
    ( traverseChannelVecWithWitness
        (\witness channelValue -> Const (foldValue witness channelValue))
        channelVecValue
    )

foldMapZipChannelVecWithWitness ::
  Monoid m =>
  (forall channel. ChannelWitness channel -> f channel -> g channel -> m) ->
  ChannelVec f ->
  ChannelVec g ->
  m
foldMapZipChannelVecWithWitness foldValue left right =
  getConst
    ( traverseChannelVecWithWitness
        ( \witness leftValue ->
            Const (foldValue witness leftValue (indexChannelVec witness right))
        )
        left
    )

indexChannelVec ::
  ChannelWitness channel ->
  ChannelVec f ->
  f channel
indexChannelVec witness channelVecValue =
  case witness of
    LoreWitness -> channelLore channelVecValue
    ClimateWitness -> channelClimate channelVecValue
    InfluenceWitness -> channelInfluence channelVecValue
    PopulationsWitness -> channelPopulations channelVecValue
    TagsWitness -> channelTags channelVecValue
    RulesWitness -> channelRules channelVecValue

replaceChannelVec ::
  ChannelWitness channel ->
  f channel ->
  ChannelVec f ->
  ChannelVec f
replaceChannelVec witness replacement channelVecValue =
  case witness of
    LoreWitness ->
      channelVecValue {channelLore = replacement}
    ClimateWitness ->
      channelVecValue {channelClimate = replacement}
    InfluenceWitness ->
      channelVecValue {channelInfluence = replacement}
    PopulationsWitness ->
      channelVecValue {channelPopulations = replacement}
    TagsWitness ->
      channelVecValue {channelTags = replacement}
    RulesWitness ->
      channelVecValue {channelRules = replacement}
