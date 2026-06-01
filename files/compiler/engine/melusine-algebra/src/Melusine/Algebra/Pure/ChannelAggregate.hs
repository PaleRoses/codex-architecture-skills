{-# LANGUAGE StandaloneDeriving #-}

module Melusine.Algebra.Pure.ChannelAggregate
  ( ChannelStateAt (..),
    ChannelDeltaAt (..),
    ChannelStateVec,
    ChannelDeltaVec,
    deltaToChannelDeltaVec,
    runDeltaProgram,
  )
where

import Data.Function ((&))
import Data.Monoid (All (..))
import Prelude hiding (Monoid, Semigroup)
import Melusine.Algebra.Pure.ChannelDescriptor
  ( ChannelDescriptor (..),
    descriptorAt,
    tabulateChannelDescriptorsWithConstraint,
  )
import Melusine.Algebra.Pure.ChannelDelta
  ( CanonicalChannel,
    ChannelConstraintBundle,
    ChannelDeltaPayload,
    ChannelState,
    ChannelWitness (..),
    Dict (..),
    DeltaProgram (..),
    allChannelConstraints,
    SomeChannelDelta (..),
    typedChannelPayload,
    typedChannelWitness,
  )
import Melusine.Algebra.Pure.ChannelVec
  ( ChannelVec (..),
    foldMapZipChannelVecWithWitness,
    replaceChannelVec,
    tabulateChannelVec,
    zipChannelVecWithWitness,
  )
import Melusine.Algebra.Pure.EqPolicy (EqPolicy (..))
import Moonlight.Algebra (Action (..), DeltaEndo, Magma (..), Monoid (..), Semigroup)

newtype ChannelStateAt (channel :: CanonicalChannel) = ChannelStateAt
  { unChannelStateAt :: ChannelState channel
  }

deriving stock instance Eq (ChannelState channel) => Eq (ChannelStateAt channel)
deriving stock instance Show (ChannelState channel) => Show (ChannelStateAt channel)
deriving stock instance Read (ChannelState channel) => Read (ChannelStateAt channel)

newtype ChannelDeltaAt (channel :: CanonicalChannel) = ChannelDeltaAt
  { unChannelDeltaAt :: ChannelDeltaPayload channel
  }

deriving stock instance Eq (ChannelDeltaPayload channel) => Eq (ChannelDeltaAt channel)
deriving stock instance Show (ChannelDeltaPayload channel) => Show (ChannelDeltaAt channel)
deriving stock instance Read (ChannelDeltaPayload channel) => Read (ChannelDeltaAt channel)

type ChannelStateVec = ChannelVec ChannelStateAt

type ChannelDeltaVec = ChannelVec ChannelDeltaAt

data ChannelAlgebra (channel :: CanonicalChannel) = ChannelAlgebra
  { channelDeltaMagma :: Dict (Magma (ChannelDeltaPayload channel)),
    channelDeltaMonoid :: Dict (Monoid (ChannelDeltaPayload channel)),
    channelAction :: Dict (Action (ChannelDeltaPayload channel) (ChannelState channel)),
    channelStateEq :: Dict (EqPolicy (ChannelState channel))
  }

class
  ( Magma (ChannelDeltaPayload channel),
    Monoid (ChannelDeltaPayload channel),
    Action (ChannelDeltaPayload channel) (ChannelState channel),
    EqPolicy (ChannelState channel)
  ) =>
  ChannelAggregateConstraint channel

instance
  ( Magma (ChannelDeltaPayload channel),
    Monoid (ChannelDeltaPayload channel),
    Action (ChannelDeltaPayload channel) (ChannelState channel),
    EqPolicy (ChannelState channel)
  ) =>
  ChannelAggregateConstraint channel

allChannelAggregateConstraints :: ChannelConstraintBundle ChannelAggregateConstraint
allChannelAggregateConstraints = allChannelConstraints @ChannelAggregateConstraint

channelAlgebraRegistry :: ChannelVec (ChannelDescriptor ChannelAlgebra)
channelAlgebraRegistry =
  tabulateChannelDescriptorsWithConstraint
    allChannelAggregateConstraints
    (\(_ :: ChannelWitness channel) -> ChannelAlgebra Dict Dict Dict Dict)

channelAlgebraAt :: ChannelWitness channel -> ChannelAlgebra channel
channelAlgebraAt witness =
  descriptorPayload (descriptorAt witness channelAlgebraRegistry)

instance Magma ChannelDeltaVec where
  magmaOp =
    zipChannelVecWithWitness
      (\witness left right ->
         case channelDeltaMagma (channelAlgebraAt witness) of
           Dict ->
             ChannelDeltaAt
               (magmaOp (unChannelDeltaAt left) (unChannelDeltaAt right))
      )

instance Semigroup ChannelDeltaVec

instance Monoid ChannelDeltaVec where
  monoidIdentity =
    tabulateChannelVec
      (\witness ->
         case channelDeltaMonoid (channelAlgebraAt witness) of
           Dict ->
             ChannelDeltaAt monoidIdentity
      )

instance Action ChannelDeltaVec ChannelStateVec where
  act =
    zipChannelVecWithWitness
      (\witness deltaValue stateValue ->
         case channelAction (channelAlgebraAt witness) of
           Dict ->
             ChannelStateAt
               (act (unChannelDeltaAt deltaValue) (unChannelStateAt stateValue))
      )

instance DeltaEndo ChannelDeltaVec ChannelStateVec

instance EqPolicy ChannelStateVec where
  eqPolicy left right =
    foldMapZipChannelVecWithWitness
      (\witness leftValue rightValue ->
         case channelStateEq (channelAlgebraAt witness) of
           Dict ->
             All
               (eqPolicy (unChannelStateAt leftValue) (unChannelStateAt rightValue))
      )
      left
      right
      & getAll

deltaToChannelDeltaVec :: SomeChannelDelta -> ChannelDeltaVec
deltaToChannelDeltaVec someDelta =
  case someDelta of
    SomeChannelDelta typedDelta ->
      replaceChannelVec
        (typedChannelWitness typedDelta)
        (ChannelDeltaAt (typedChannelPayload typedDelta))
        monoidIdentity

runDeltaProgram :: DeltaProgram -> ChannelStateVec -> ChannelStateVec
runDeltaProgram (DeltaProgram deltas) initialState =
  deltas
    & foldr
      (\deltaValue continuation -> continuation . act (deltaToChannelDeltaVec deltaValue))
      id
    $ initialState
