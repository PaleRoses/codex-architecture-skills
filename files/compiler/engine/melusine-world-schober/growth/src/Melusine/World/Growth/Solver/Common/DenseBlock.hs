{-# LANGUAGE BangPatterns #-}

module Melusine.World.Growth.Solver.Common.DenseBlock
  ( blockResidualInto
  , blockDotM
  , blockMixInto
  , blockMaxAbsDeltaM
  , blockMaxAbsDelta
  ) where

import Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

blockResidualInto
  :: (VUM.Unbox a, Num a)
  => Int -> VUM.MVector s a -> VUM.MVector s a -> VUM.MVector s a -> ST s ()
blockResidualInto !n !out !a !b = go 0
  where
    go !ix
      | ix == n = pure ()
      | otherwise = do
          xa <- VUM.unsafeRead a ix
          xb <- VUM.unsafeRead b ix
          VUM.unsafeWrite out ix (xa - xb)
          go (ix + 1)
{-# INLINE blockResidualInto #-}

blockDotM
  :: (VUM.Unbox a, Num a)
  => Int -> VUM.MVector s a -> VUM.MVector s a -> ST s a
blockDotM !n !a !b = go 0 0
  where
    go !ix !acc
      | ix == n = pure acc
      | otherwise = do
          xa <- VUM.unsafeRead a ix
          xb <- VUM.unsafeRead b ix
          go (ix + 1) (acc + xa * xb)
{-# INLINE blockDotM #-}

blockMixInto
  :: (VUM.Unbox a, Num a)
  => a -> Int -> VUM.MVector s a -> a -> VUM.MVector s a -> VUM.MVector s a -> ST s ()
blockMixInto !wa !n !out !wb !a !b = go 0
  where
    go !ix
      | ix == n = pure ()
      | otherwise = do
          xa <- VUM.unsafeRead a ix
          xb <- VUM.unsafeRead b ix
          VUM.unsafeWrite out ix (wa * xa + wb * xb)
          go (ix + 1)
{-# INLINE blockMixInto #-}

blockMaxAbsDeltaM
  :: (VUM.Unbox a, Num a, Ord a)
  => Int -> VUM.MVector s a -> VUM.MVector s a -> ST s a
blockMaxAbsDeltaM !n !a !b = go 0 0
  where
    go !ix !best
      | ix == n = pure best
      | otherwise = do
          xa <- VUM.unsafeRead a ix
          xb <- VUM.unsafeRead b ix
          go (ix + 1) (max best (abs (xa - xb)))
{-# INLINE blockMaxAbsDeltaM #-}

blockMaxAbsDelta
  :: (VU.Unbox a, Num a, Ord a)
  => Int -> VU.Vector a -> VU.Vector a -> a
blockMaxAbsDelta !n !a !b =
  let go !ix !best
        | ix == n = best
        | otherwise =
            let !d = abs (VU.unsafeIndex a ix - VU.unsafeIndex b ix)
            in go (ix + 1) (max best d)
  in go 0 0
{-# INLINE blockMaxAbsDelta #-}
