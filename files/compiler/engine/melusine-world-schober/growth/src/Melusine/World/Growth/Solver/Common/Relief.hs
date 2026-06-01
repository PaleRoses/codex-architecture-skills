{-# LANGUAGE BangPatterns #-}

module Melusine.World.Growth.Solver.Common.Relief
  ( ReliefBlock(..)
  , MReliefBlock(..)
  , reliefRows
  , reliefData
  , reliefReplicate
  , reliefGenerate
  , newReliefMutable
  , thawRelief
  , freezeRelief
  , freezeReliefClone
  , reliefAt
  , reliefReadM
  , reliefWrite
  , reliefAdd
  , reliefCopy
  , reliefCopyFaces
  , reliefFillFaces
  , reliefResidualInto
  , reliefDotM
  , reliefMixInto
  , reliefMaxAbsDeltaM
  , reliefMaxAbsDeltaFacesM
  , reliefMaxAbsDelta
  ) where

import Control.Monad.ST (ST, runST)
import Data.Kind (Type)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Melusine.World.Growth.Internal.Channels
  ( ScalarField
  )
import Melusine.World.Growth.Solver.Common.DenseBlock
  ( blockResidualInto
  , blockDotM
  , blockMixInto
  , blockMaxAbsDeltaM
  , blockMaxAbsDelta
  )

type ReliefBlock :: Type
data ReliefBlock = ReliefBlock
  { rbRows :: !Int
  , rbData :: !ScalarField
  }

type MReliefBlock :: Type -> Type
data MReliefBlock s = MReliefBlock
  { mrbRows :: !Int
  , mrbData :: !(VUM.MVector s Double)
  }

reliefRows :: ReliefBlock -> Int
reliefRows = rbRows
{-# INLINE reliefRows #-}

reliefData :: ReliefBlock -> ScalarField
reliefData = rbData
{-# INLINE reliefData #-}

reliefReplicate :: Int -> Double -> ReliefBlock
reliefReplicate !rows !x =
  ReliefBlock rows (VU.replicate rows x)

reliefGenerate :: Int -> (Int -> Double) -> ReliefBlock
reliefGenerate !rows !f = runST $ do
  out <- VUM.unsafeNew rows
  let go !i
        | i == rows = pure ()
        | otherwise = do
            VUM.unsafeWrite out i (f i)
            go (i + 1)
  go 0
  ReliefBlock rows <$> VU.unsafeFreeze out

newReliefMutable :: Int -> Double -> ST s (MReliefBlock s)
newReliefMutable !rows !x =
  MReliefBlock rows <$> VUM.replicate rows x

thawRelief :: ReliefBlock -> ST s (MReliefBlock s)
thawRelief (ReliefBlock rows dat) =
  MReliefBlock rows <$> VU.thaw dat

freezeRelief :: MReliefBlock s -> ST s ReliefBlock
freezeRelief (MReliefBlock rows dat) =
  ReliefBlock rows <$> VU.unsafeFreeze dat

freezeReliefClone :: MReliefBlock s -> ST s ReliefBlock
freezeReliefClone (MReliefBlock rows dat) =
  ReliefBlock rows <$> VU.freeze dat

reliefAt :: ReliefBlock -> Int -> Double
reliefAt (ReliefBlock _ dat) !i =
  VU.unsafeIndex dat i
{-# INLINE reliefAt #-}

reliefReadM :: MReliefBlock s -> Int -> ST s Double
reliefReadM (MReliefBlock _ dat) !i =
  VUM.unsafeRead dat i
{-# INLINE reliefReadM #-}

reliefWrite :: MReliefBlock s -> Int -> Double -> ST s ()
reliefWrite (MReliefBlock _ dat) !i !x =
  VUM.unsafeWrite dat i x
{-# INLINE reliefWrite #-}

reliefAdd :: MReliefBlock s -> Int -> Double -> ST s ()
reliefAdd !blk !i !dx = do
  x0 <- reliefReadM blk i
  reliefWrite blk i (x0 + dx)
{-# INLINE reliefAdd #-}

reliefCopy :: MReliefBlock s -> MReliefBlock s -> ST s ()
reliefCopy (MReliefBlock _ dst) (MReliefBlock _ src) =
  VUM.unsafeCopy dst src
{-# INLINE reliefCopy #-}

reliefCopyFaces :: VU.Vector Int -> MReliefBlock s -> MReliefBlock s -> ST s ()
reliefCopyFaces !faces (MReliefBlock _ dst) (MReliefBlock _ src) = do
  let !count = VU.length faces
      go !ix
        | ix == count = pure ()
        | otherwise = do
            let !i = VU.unsafeIndex faces ix
            x <- VUM.unsafeRead src i
            VUM.unsafeWrite dst i x
            go (ix + 1)
  go 0
{-# INLINE reliefCopyFaces #-}

reliefFillFaces :: VU.Vector Int -> MReliefBlock s -> Double -> ST s ()
reliefFillFaces !faces (MReliefBlock _ dst) !x = do
  let !count = VU.length faces
      go !ix
        | ix == count = pure ()
        | otherwise = do
            VUM.unsafeWrite dst (VU.unsafeIndex faces ix) x
            go (ix + 1)
  go 0
{-# INLINE reliefFillFaces #-}

reliefResidualInto :: MReliefBlock s -> MReliefBlock s -> MReliefBlock s -> ST s ()
reliefResidualInto (MReliefBlock rows out) (MReliefBlock _ a) (MReliefBlock _ b) =
  blockResidualInto rows out a b

reliefDotM :: MReliefBlock s -> MReliefBlock s -> ST s Double
reliefDotM (MReliefBlock rows a) (MReliefBlock _ b) =
  blockDotM rows a b

reliefMixInto :: Double -> MReliefBlock s -> Double -> MReliefBlock s -> MReliefBlock s -> ST s ()
reliefMixInto !wa (MReliefBlock rows out) !wb (MReliefBlock _ a) (MReliefBlock _ b) =
  blockMixInto wa rows out wb a b

reliefMaxAbsDeltaM :: MReliefBlock s -> MReliefBlock s -> ST s Double
reliefMaxAbsDeltaM (MReliefBlock rows a) (MReliefBlock _ b) =
  blockMaxAbsDeltaM rows a b

reliefMaxAbsDeltaFacesM :: VU.Vector Int -> MReliefBlock s -> MReliefBlock s -> ST s Double
reliefMaxAbsDeltaFacesM !faces (MReliefBlock _ a) (MReliefBlock _ b) = do
  let !count = VU.length faces
      go !ix !best
        | ix == count = pure best
        | otherwise = do
            let !i = VU.unsafeIndex faces ix
            xa <- VUM.unsafeRead a i
            xb <- VUM.unsafeRead b i
            go (ix + 1) (max best (abs (xa - xb)))
  go 0 0.0

reliefMaxAbsDelta :: ReliefBlock -> ReliefBlock -> Double
reliefMaxAbsDelta (ReliefBlock rows a) (ReliefBlock _ b) =
  blockMaxAbsDelta rows a b
