

module Melusine.Sheaf.Site.Stalk
  ( StalkState (..),
    Stalk,
    defaultStalk,
    channelDefaultFromWitness,
    lookupStalkChannel,
    updateStalkChannel,
    applySomeChannelDeltaToStalk,
    StalkDelta (..),
    StalkDeltaVec,
    identityStalkDelta,
    composeStalkDelta,
    applyStalkDelta,
    channelEqPolicy,
    foldMapStalkChannels,
    stalkMismatchedChannels,
    stalkApproxEq,
    stalkAlgebra,
  )
where

import Data.Monoid (All (..))
import Melusine.Algebra
  ( CanonicalChannel,
    ChannelDeltaPayload,
    ChannelState,
    ChannelVec,
    SomeChannelDelta (..),
    ChannelWitness (..),
    applyTypedChannelDelta,
    channelIdFromWitness,
    channelDefault,
    channelDeltaRegistry,
    eqPolicy,
    foldMapZipChannelVecWithWitness,
    indexChannelVec,
    registryApplyDelta,
    registryComposeDelta,
    registryIdentityDelta,
    replaceChannelVec,
    tabulateChannelVec,
    typedChannelWitness,
    zipChannelVecWithWitness,
  )
import Melusine.Sheaf.Site.ChannelMismatch (ChannelMismatchKey (..))
import Moonlight.Sheaf.Section.Stalk qualified as Pure
newtype StalkState (channel :: CanonicalChannel) = StalkState
  { unStalkState :: ChannelState channel
  }

type Stalk = ChannelVec StalkState

defaultStalk :: Stalk
defaultStalk =
  tabulateChannelVec
    (\witness -> StalkState (channelDefaultFromWitness witness))

channelDefaultFromWitness :: ChannelWitness channel -> ChannelState channel
channelDefaultFromWitness witness =
  case witness of
    LoreWitness -> channelDefault
    ClimateWitness -> channelDefault
    InfluenceWitness -> channelDefault
    PopulationsWitness -> channelDefault
    TagsWitness -> channelDefault
    RulesWitness -> channelDefault

lookupStalkChannel ::
  ChannelWitness channel ->
  Stalk ->
  ChannelState channel
lookupStalkChannel witness stalk =
  unStalkState (indexChannelVec witness stalk)

updateStalkChannel ::
  ChannelWitness channel ->
  ChannelState channel ->
  Stalk ->
  Stalk
updateStalkChannel witness value stalk =
  replaceChannelVec witness (StalkState value) stalk

applySomeChannelDeltaToStalk :: SomeChannelDelta -> Stalk -> Stalk
applySomeChannelDeltaToStalk (SomeChannelDelta typedDelta) stalk =
  let witness = typedChannelWitness typedDelta
      currentState = lookupStalkChannel witness stalk
      nextState = applyTypedChannelDelta typedDelta currentState
   in updateStalkChannel witness nextState stalk

newtype StalkDelta (channel :: CanonicalChannel) = StalkDelta
  { unStalkDelta :: ChannelDeltaPayload channel
  }

type StalkDeltaVec = ChannelVec StalkDelta

identityStalkDelta :: StalkDeltaVec
identityStalkDelta =
  tabulateChannelVec
    (\witness -> StalkDelta (registryIdentityDelta (channelDeltaRegistry witness)))

composeStalkDelta ::
  StalkDeltaVec ->
  StalkDeltaVec ->
  StalkDeltaVec
composeStalkDelta left right =
  zipChannelVecWithWitness
    (\witness (StalkDelta leftPayload) (StalkDelta rightPayload) ->
       StalkDelta
         (registryComposeDelta (channelDeltaRegistry witness) leftPayload rightPayload)
    )
    left
    right

applyStalkDelta ::
  StalkDeltaVec ->
  Stalk ->
  Stalk
applyStalkDelta delta stalk =
  zipChannelVecWithWitness
    (\witness (StalkDelta deltaPayload) (StalkState stateValue) ->
       StalkState
         (registryApplyDelta (channelDeltaRegistry witness) deltaPayload stateValue)
    )
    delta
    stalk

channelEqPolicy ::
  ChannelWitness channel ->
  ChannelState channel ->
  ChannelState channel ->
  Bool
channelEqPolicy witness =
  case witness of
    LoreWitness -> eqPolicy
    ClimateWitness -> eqPolicy
    InfluenceWitness -> eqPolicy
    PopulationsWitness -> eqPolicy
    TagsWitness -> eqPolicy
    RulesWitness -> eqPolicy

foldMapStalkChannels ::
  Monoid m =>
  (forall channel. ChannelWitness channel -> ChannelState channel -> ChannelState channel -> m) ->
  Stalk ->
  Stalk ->
  m
foldMapStalkChannels foldChannel left right =
  foldMapZipChannelVecWithWitness
    ( \witness (StalkState leftState) (StalkState rightState) ->
        foldChannel witness leftState rightState
    )
    left
    right

stalkMismatchedChannels :: Stalk -> Stalk -> [ChannelMismatchKey]
stalkMismatchedChannels left right =
  foldMapStalkChannels
    (\witness leftState rightState ->
       if channelEqPolicy witness leftState rightState
         then []
         else [ChannelMismatchKey (channelIdFromWitness witness)]
    )
    left
    right

stalkApproxEq :: Stalk -> Stalk -> Bool
stalkApproxEq left right =
  getAll
    ( foldMapStalkChannels
        (\witness leftState rightState -> All (channelEqPolicy witness leftState rightState))
        left
        right
    )

stalkAlgebra :: Pure.StalkAlgebra witness Stalk ChannelMismatchKey ()
stalkAlgebra =
  Pure.StalkAlgebra
    { Pure.saRestrict = const id,
      Pure.saMismatches = stalkMismatchedChannels,
      Pure.saMerge = mergeStalkContribution,
      Pure.saRepair = const (Left ()),
      Pure.saNormalize = id
    }

mergeStalkContribution :: Stalk -> Stalk -> Either (Pure.MergeObstruction ChannelMismatchKey) Stalk
mergeStalkContribution left right =
  case Pure.mismatchObstruction (stalkMismatchedChannels left right) of
    Just obstruction ->
      Left obstruction
    Nothing ->
      Right left
