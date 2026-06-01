{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.World.Growth.Internal.Channels
  ( ScalarField
  , EdgeField
  , TransportField
  , Channels(..)
  , channelsEmpty
  , channelsGenerate
  , channelsFromRows
  , channelsAt
  , channelsRowStart
  , channelsMap
  , channelsZipWith
  , channelsAverageMass
  , channelsArgmaxLabels
  , channelsSoftmax
  , blendPrototypes
  ) where

import Control.Monad.ST (runST)
import Data.Kind (Type)
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Melusine.World.Growth.Internal.Util (idx2)

type ScalarField :: Type
type ScalarField = VU.Vector Double
type EdgeField :: Type
type EdgeField = VU.Vector Double
type TransportField :: Type
type TransportField = VU.Vector Float

type Channels :: Type
data Channels = Channels
  { chRows :: !Int
  , chCols :: !Int
  , chData :: !(VU.Vector Float)
  }

channelsEmpty :: Int -> Int -> Channels
channelsEmpty !rows !cols = Channels rows cols (VU.replicate (rows * cols) (0.0 :: Float))

channelsGenerate :: Int -> Int -> (Int -> Int -> Float) -> Channels
channelsGenerate !rows !cols !f =
  Channels rows cols $
    VU.generate (rows * cols) $ \ix ->
      let (!i, !j) = ix `quotRem` cols
      in f i j

channelsFromRows :: Int -> Int -> VB.Vector ScalarField -> Channels
channelsFromRows !rows !cols !rowVec = runST $ do
  out <- VUM.unsafeNew (rows * cols)
  let faceLoop !i
        | i == rows = pure ()
        | otherwise = do
            let !row =
                  if i < VB.length rowVec
                    then VB.unsafeIndex rowVec i
                    else VU.replicate cols 0.0
                fill !c
                  | c == cols = pure ()
                  | otherwise = do
                      let !v = if c < VU.length row then VU.unsafeIndex row c else 0.0
                      VUM.unsafeWrite out (idx2 i c cols) (realToFrac v)
                      fill (c + 1)
            fill 0
            faceLoop (i + 1)
  faceLoop 0
  dat <- VU.unsafeFreeze out
  pure (Channels rows cols dat)

channelsAt :: Channels -> Int -> Int -> Float
channelsAt !ch !i !c = VU.unsafeIndex (chData ch) (idx2 i c (chCols ch))
{-# INLINE channelsAt #-}

channelsRowStart :: Channels -> Int -> Int
channelsRowStart !ch !i = i * chCols ch
{-# INLINE channelsRowStart #-}

channelsMap :: (Float -> Float) -> Channels -> Channels
channelsMap !f !ch = ch { chData = VU.map f (chData ch) }

channelsZipWith :: (Float -> Float -> Float) -> Channels -> Channels -> Channels
channelsZipWith !f !a !b =
  Channels (chRows a) (chCols a) (VU.zipWith f (chData a) (chData b))

channelsAverageMass :: Channels -> VU.Vector Float
channelsAverageMass !ch
  | chCols ch <= 0 = VU.empty
  | otherwise =
      VU.generate (chCols ch) $ \c ->
        let go !i !acc
              | i == chRows ch = acc
              | otherwise = go (i + 1) (acc + channelsAt ch i c)
        in go 0 (0.0 :: Float) / max 1.0 (fromIntegral (chRows ch))

channelsArgmaxLabels :: Channels -> VU.Vector Int
channelsArgmaxLabels !ch
  | chCols ch <= 0 = VU.replicate (chRows ch) 0
  | otherwise =
      VU.generate (chRows ch) $ \i ->
        let !v0 = channelsAt ch i 0
            go !c !bestC !bestV
              | c == chCols ch = bestC
              | otherwise =
                  let !v = channelsAt ch i c
                  in if v > bestV
                       then go (c + 1) c v
                       else go (c + 1) bestC bestV
        in go 1 0 v0

channelsSoftmax :: Channels -> Channels
channelsSoftmax !ch
  | chCols ch <= 0 = ch
  | otherwise = runST $ do
      out <- VUM.unsafeNew (chRows ch * chCols ch)
      let rows = chRows ch
          cols = chCols ch
          dat = chData ch
          faceLoop !i
            | i == rows = pure ()
            | otherwise = do
                let !base = i * cols
                    maxLoop !c !currentMax
                      | c == cols = currentMax
                      | otherwise = maxLoop (c + 1) (max currentMax (VU.unsafeIndex dat (base + c)))
                    !rowMax = maxLoop 0 (-1 / 0 :: Float)
                    expLoop !c !z
                      | c == cols = pure z
                      | otherwise = do
                          let !e = exp (VU.unsafeIndex dat (base + c) - rowMax)
                          VUM.unsafeWrite out (base + c) e
                          expLoop (c + 1) (z + e)
                !z <- expLoop 0 (0.0 :: Float)
                let !invZ = 1.0 / max 1.0e-12 z
                    normLoop !c
                      | c == cols = pure ()
                      | otherwise = do
                          x <- VUM.unsafeRead out (base + c)
                          VUM.unsafeWrite out (base + c) (x * invZ)
                          normLoop (c + 1)
                normLoop 0
                faceLoop (i + 1)
      faceLoop 0
      dat' <- VU.unsafeFreeze out
      pure (Channels (chRows ch) (chCols ch) dat')

blendPrototypes :: Channels -> Channels -> Channels
blendPrototypes !weights !prototypes
  | chCols weights <= 0 || chRows prototypes <= 0 = channelsEmpty (chRows weights) (chCols prototypes)
  | otherwise =
      channelsGenerate (chRows weights) (chCols prototypes) $ \i a ->
        let go !k !acc
              | k == chCols weights = acc
              | otherwise =
                  go (k + 1) (acc + channelsAt weights i k * channelsAt prototypes k a)
        in go 0 (0.0 :: Float)
