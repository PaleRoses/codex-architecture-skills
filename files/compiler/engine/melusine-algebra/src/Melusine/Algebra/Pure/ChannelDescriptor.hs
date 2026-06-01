{-# LANGUAGE ConstraintKinds #-}

module Melusine.Algebra.Pure.ChannelDescriptor
  ( ChannelDescriptor (..),
    tabulateChannelDescriptors,
    tabulateChannelDescriptorsWithConstraint,
    descriptorAt,
  )
where

import Data.Kind (Type)
import Melusine.Algebra.Pure.Channel (ChannelId)
import Melusine.Algebra.Pure.ChannelDelta
  ( CanonicalChannel,
    ChannelConstraintBundle,
    ChannelWitness,
    channelIdFromWitness,
  )
import Melusine.Algebra.Pure.ChannelVec
  ( ChannelVec,
    indexChannelVec,
    tabulateChannelVec,
    tabulateChannelVecWithConstraint,
  )

data ChannelDescriptor (payload :: CanonicalChannel -> Type) (channel :: CanonicalChannel) = ChannelDescriptor
  { descriptorId :: ChannelId,
    descriptorPayload :: payload channel
  }

tabulateChannelDescriptors ::
  (forall channel. ChannelWitness channel -> payload channel) ->
  ChannelVec (ChannelDescriptor payload)
tabulateChannelDescriptors buildPayload =
  tabulateChannelVec
    (\witness ->
       ChannelDescriptor
         { descriptorId = channelIdFromWitness witness,
           descriptorPayload = buildPayload witness
         }
    )

tabulateChannelDescriptorsWithConstraint ::
  ChannelConstraintBundle constraint ->
  (forall channel. constraint channel => ChannelWitness channel -> payload channel) ->
  ChannelVec (ChannelDescriptor payload)
tabulateChannelDescriptorsWithConstraint constraints buildPayload =
  tabulateChannelVecWithConstraint
    constraints
    (\witness ->
       ChannelDescriptor
         { descriptorId = channelIdFromWitness witness,
           descriptorPayload = buildPayload witness
         }
    )

descriptorAt ::
  ChannelWitness channel ->
  ChannelVec (ChannelDescriptor payload) ->
  ChannelDescriptor payload channel
descriptorAt = indexChannelVec
