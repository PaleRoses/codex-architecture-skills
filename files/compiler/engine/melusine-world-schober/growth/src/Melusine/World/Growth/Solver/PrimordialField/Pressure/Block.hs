{-# LANGUAGE BangPatterns #-}

module Melusine.World.Growth.Solver.PrimordialField.Pressure.Block
  ( Pressure8(..)
  , PressureInput(..)
  , diagMul8
  , pressureInputsPull
  , pressureInputsAt
  , PressureBlock(..)
  , MPressureBlock(..)
  , pressureRows
  , pressureData
  , pressureReplicate
  , pressureGenerate
  , newPressureMutable
  , thawPressure
  , freezePressure
  , pressureAt
  , pressureRead8
  , pressureWrite8
  , pressureAdd8
  , pressureAtM
  , pressureRead8M
  , pressureCopy
  , pressureCopyFaces
  , pressureFillFaces
  , freezePressureClone
  , pressureResidualInto
  , pressureDotM
  , pressureMixInto
  , pressureMaxAbsDeltaM
  , pressureMaxAbsDeltaFacesM
  , pressureMaxAbsDelta
  , pressureResidual
  , pressureDot
  , pressureMix
  , add8
  , sub8
  , scale8
  , tanh8
  , maxAbs8
  , pressure8FromChannelsRow
  , applyMatrix8
  , pressureFaceClimate
  ) where

import Data.Kind (Type)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Melusine.World.Growth.Internal.Channels
  ( Channels(..)
  , ScalarField
  , channelsAt
  )
import Melusine.World.Growth.Solver.Common.DenseBlock
  ( blockResidualInto
  , blockDotM
  , blockMixInto
  , blockMaxAbsDeltaM
  , blockMaxAbsDelta
  )
import Melusine.Algebra (ClimateVec(..))

type Pressure8 :: Type
data Pressure8 = Pressure8
  !Float !Float !Float !Float !Float !Float !Float !Float

add8 :: Pressure8 -> Pressure8 -> Pressure8
add8 (Pressure8 a0 a1 a2 a3 a4 a5 a6 a7) (Pressure8 b0 b1 b2 b3 b4 b5 b6 b7) =
  Pressure8 (a0 + b0) (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4) (a5 + b5) (a6 + b6) (a7 + b7)
{-# INLINE add8 #-}

sub8 :: Pressure8 -> Pressure8 -> Pressure8
sub8 (Pressure8 a0 a1 a2 a3 a4 a5 a6 a7) (Pressure8 b0 b1 b2 b3 b4 b5 b6 b7) =
  Pressure8 (a0 - b0) (a1 - b1) (a2 - b2) (a3 - b3) (a4 - b4) (a5 - b5) (a6 - b6) (a7 - b7)
{-# INLINE sub8 #-}

scale8 :: Float -> Pressure8 -> Pressure8
scale8 !s (Pressure8 a0 a1 a2 a3 a4 a5 a6 a7) =
  Pressure8 (s * a0) (s * a1) (s * a2) (s * a3) (s * a4) (s * a5) (s * a6) (s * a7)
{-# INLINE scale8 #-}

tanh8 :: Pressure8 -> Pressure8
tanh8 (Pressure8 a0 a1 a2 a3 a4 a5 a6 a7) =
  Pressure8 (tanh a0) (tanh a1) (tanh a2) (tanh a3) (tanh a4) (tanh a5) (tanh a6) (tanh a7)
{-# INLINE tanh8 #-}

maxAbs8 :: Pressure8 -> Float
maxAbs8 (Pressure8 a0 a1 a2 a3 a4 a5 a6 a7) =
  max (abs a0)
    (max (abs a1)
      (max (abs a2)
        (max (abs a3)
          (max (abs a4)
            (max (abs a5)
              (max (abs a6) (abs a7)))))))
{-# INLINE maxAbs8 #-}

type PressureInput :: Type -> Type
data PressureInput s = PressureInput
  { piPull  :: !ScalarField
  , piBlend :: !(MPressureBlock s)
  }

diagMul8 :: VU.Vector Double -> Pressure8 -> Pressure8
diagMul8 !d (Pressure8 x0 x1 x2 x3 x4 x5 x6 x7) =
  let f !i = realToFrac (VU.unsafeIndex d i) :: Float
  in Pressure8
      (f 0 * x0) (f 1 * x1) (f 2 * x2) (f 3 * x3)
      (f 4 * x4) (f 5 * x5) (f 6 * x6) (f 7 * x7)
{-# INLINE diagMul8 #-}

pressureInputsPull :: [PressureInput s] -> VU.Vector Double
pressureInputsPull =
  foldl'
    (\ !acc !inp -> VU.zipWith (+) acc (piPull inp))
    (VU.replicate 8 0.0)
{-# INLINE pressureInputsPull #-}

pressureInputsAt :: [PressureInput s] -> Int -> ST s Pressure8
pressureInputsAt !inputs !i = go (Pressure8 0 0 0 0 0 0 0 0) inputs
  where
    go !acc [] = pure acc
    go !acc (PressureInput pull blend : rest) = do
      x <- diagMul8 pull <$> pressureRead8M blend i
      go (add8 acc x) rest
{-# INLINE pressureInputsAt #-}

type PressureBlock :: Type
data PressureBlock = PressureBlock
  { pbRows :: !Int
  , pbData :: !(VU.Vector Float)
  }

type MPressureBlock :: Type -> Type
data MPressureBlock s = MPressureBlock
  { mpbRows :: !Int
  , mpbData :: !(VUM.MVector s Float)
  }

pressureRows :: PressureBlock -> Int
pressureRows = pbRows
{-# INLINE pressureRows #-}

pressureData :: PressureBlock -> VU.Vector Float
pressureData = pbData
{-# INLINE pressureData #-}

pressureIx :: Int -> Int -> Int -> Int
pressureIx !rows !a !i = a * rows + i
{-# INLINE pressureIx #-}

pressureReplicate :: Int -> Float -> PressureBlock
pressureReplicate !rows !x =
  PressureBlock rows (VU.replicate (rows * 8) x)

pressureGenerate :: Int -> (Int -> Pressure8) -> PressureBlock
pressureGenerate !rows !f = runST $ do
  out <- VUM.unsafeNew (rows * 8)
  let faceLoop !i
        | i == rows = pure ()
        | otherwise = do
            let !(Pressure8 a0 a1 a2 a3 a4 a5 a6 a7) = f i
            VUM.unsafeWrite out (pressureIx rows 0 i) a0
            VUM.unsafeWrite out (pressureIx rows 1 i) a1
            VUM.unsafeWrite out (pressureIx rows 2 i) a2
            VUM.unsafeWrite out (pressureIx rows 3 i) a3
            VUM.unsafeWrite out (pressureIx rows 4 i) a4
            VUM.unsafeWrite out (pressureIx rows 5 i) a5
            VUM.unsafeWrite out (pressureIx rows 6 i) a6
            VUM.unsafeWrite out (pressureIx rows 7 i) a7
            faceLoop (i + 1)
  faceLoop 0
  dat <- VU.unsafeFreeze out
  pure (PressureBlock rows dat)

newPressureMutable :: Int -> Float -> ST s (MPressureBlock s)
newPressureMutable !rows !x = do
  dat <- VUM.replicate (rows * 8) x
  pure (MPressureBlock rows dat)

thawPressure :: PressureBlock -> ST s (MPressureBlock s)
thawPressure (PressureBlock rows dat) = do
  mv <- VU.thaw dat
  pure (MPressureBlock rows mv)

freezePressure :: MPressureBlock s -> ST s PressureBlock
freezePressure (MPressureBlock rows dat) = do
  frozen <- VU.unsafeFreeze dat
  pure (PressureBlock rows frozen)

pressureAt :: PressureBlock -> Int -> Int -> Float
pressureAt (PressureBlock rows dat) !i !a =
  VU.unsafeIndex dat (pressureIx rows a i)
{-# INLINE pressureAt #-}

pressureRead8 :: PressureBlock -> Int -> Pressure8
pressureRead8 (PressureBlock rows dat) !i =
  Pressure8
    (VU.unsafeIndex dat (pressureIx rows 0 i))
    (VU.unsafeIndex dat (pressureIx rows 1 i))
    (VU.unsafeIndex dat (pressureIx rows 2 i))
    (VU.unsafeIndex dat (pressureIx rows 3 i))
    (VU.unsafeIndex dat (pressureIx rows 4 i))
    (VU.unsafeIndex dat (pressureIx rows 5 i))
    (VU.unsafeIndex dat (pressureIx rows 6 i))
    (VU.unsafeIndex dat (pressureIx rows 7 i))
{-# INLINE pressureRead8 #-}

pressureWrite8 :: MPressureBlock s -> Int -> Pressure8 -> ST s ()
pressureWrite8 (MPressureBlock rows dat) !i (Pressure8 a0 a1 a2 a3 a4 a5 a6 a7) = do
  VUM.unsafeWrite dat (pressureIx rows 0 i) a0
  VUM.unsafeWrite dat (pressureIx rows 1 i) a1
  VUM.unsafeWrite dat (pressureIx rows 2 i) a2
  VUM.unsafeWrite dat (pressureIx rows 3 i) a3
  VUM.unsafeWrite dat (pressureIx rows 4 i) a4
  VUM.unsafeWrite dat (pressureIx rows 5 i) a5
  VUM.unsafeWrite dat (pressureIx rows 6 i) a6
  VUM.unsafeWrite dat (pressureIx rows 7 i) a7
{-# INLINE pressureWrite8 #-}

pressureAdd8 :: MPressureBlock s -> Int -> Pressure8 -> ST s ()
pressureAdd8 (MPressureBlock rows dat) !i (Pressure8 a0 a1 a2 a3 a4 a5 a6 a7) = do
  let add !a !x = do
        let !ix = pressureIx rows a i
        old <- VUM.unsafeRead dat ix
        VUM.unsafeWrite dat ix (old + x)
  add 0 a0; add 1 a1; add 2 a2; add 3 a3
  add 4 a4; add 5 a5; add 6 a6; add 7 a7
{-# INLINE pressureAdd8 #-}

pressureAtM :: MPressureBlock s -> Int -> Int -> ST s Float
pressureAtM (MPressureBlock rows dat) !i !a =
  VUM.unsafeRead dat (pressureIx rows a i)
{-# INLINE pressureAtM #-}

pressureRead8M :: MPressureBlock s -> Int -> ST s Pressure8
pressureRead8M (MPressureBlock rows dat) !i = do
  a0 <- VUM.unsafeRead dat (pressureIx rows 0 i)
  a1 <- VUM.unsafeRead dat (pressureIx rows 1 i)
  a2 <- VUM.unsafeRead dat (pressureIx rows 2 i)
  a3 <- VUM.unsafeRead dat (pressureIx rows 3 i)
  a4 <- VUM.unsafeRead dat (pressureIx rows 4 i)
  a5 <- VUM.unsafeRead dat (pressureIx rows 5 i)
  a6 <- VUM.unsafeRead dat (pressureIx rows 6 i)
  a7 <- VUM.unsafeRead dat (pressureIx rows 7 i)
  pure (Pressure8 a0 a1 a2 a3 a4 a5 a6 a7)
{-# INLINE pressureRead8M #-}

pressureCopy :: MPressureBlock s -> MPressureBlock s -> ST s ()
pressureCopy (MPressureBlock _ dst) (MPressureBlock _ src) =
  VUM.unsafeCopy dst src
{-# INLINE pressureCopy #-}

pressureCopyFaces :: VU.Vector Int -> MPressureBlock s -> MPressureBlock s -> ST s ()
pressureCopyFaces !faces !(MPressureBlock rows dst) !(MPressureBlock _ src) = do
  let !count = VU.length faces
      axisLoop !a
        | a == 8 = pure ()
        | otherwise = do
            let !base = a * rows
                faceLoop !ix
                  | ix == count = pure ()
                  | otherwise = do
                      let !i = VU.unsafeIndex faces ix
                      x <- VUM.unsafeRead src (base + i)
                      VUM.unsafeWrite dst (base + i) x
                      faceLoop (ix + 1)
            faceLoop 0
            axisLoop (a + 1)
  axisLoop 0
{-# INLINE pressureCopyFaces #-}

pressureFillFaces :: VU.Vector Int -> MPressureBlock s -> Float -> ST s ()
pressureFillFaces !faces (MPressureBlock rows dat) !x = do
  let !count = VU.length faces
      axisLoop !a
        | a == 8 = pure ()
        | otherwise = do
            let !base = a * rows
                faceLoop !ix
                  | ix == count = pure ()
                  | otherwise = do
                      VUM.unsafeWrite dat (base + VU.unsafeIndex faces ix) x
                      faceLoop (ix + 1)
            faceLoop 0
            axisLoop (a + 1)
  axisLoop 0
{-# INLINE pressureFillFaces #-}

freezePressureClone :: MPressureBlock s -> ST s PressureBlock
freezePressureClone (MPressureBlock rows dat) = do
  frozen <- VU.freeze dat
  pure (PressureBlock rows frozen)

pressureResidualInto :: MPressureBlock s -> MPressureBlock s -> MPressureBlock s -> ST s ()
pressureResidualInto (MPressureBlock rows out) (MPressureBlock _ a) (MPressureBlock _ b) =
  blockResidualInto (rows * 8) out a b

pressureDotM :: MPressureBlock s -> MPressureBlock s -> ST s Float
pressureDotM (MPressureBlock rows a) (MPressureBlock _ b) =
  blockDotM (rows * 8) a b

pressureMixInto :: Float -> MPressureBlock s -> Float -> MPressureBlock s -> MPressureBlock s -> ST s ()
pressureMixInto !wa (MPressureBlock rows out) !wb (MPressureBlock _ a) (MPressureBlock _ b) =
  blockMixInto wa (rows * 8) out wb a b

pressureMaxAbsDeltaM :: MPressureBlock s -> MPressureBlock s -> ST s Float
pressureMaxAbsDeltaM (MPressureBlock rows a) (MPressureBlock _ b) =
  blockMaxAbsDeltaM (rows * 8) a b

pressureMaxAbsDeltaFacesM :: VU.Vector Int -> MPressureBlock s -> MPressureBlock s -> ST s Float
pressureMaxAbsDeltaFacesM !faces !a !b = do
  let !count = VU.length faces
      faceLoop !ix !best
        | ix == count = pure best
        | otherwise = do
            let !i = VU.unsafeIndex faces ix
            pa <- pressureRead8M a i
            pb <- pressureRead8M b i
            faceLoop (ix + 1) (max best (maxAbs8 (sub8 pa pb)))
  faceLoop 0 0.0

pressureResidual :: PressureBlock -> PressureBlock -> PressureBlock
pressureResidual (PressureBlock rows newDat) (PressureBlock _ oldDat) =
  PressureBlock rows (VU.zipWith (-) newDat oldDat)

pressureDot :: PressureBlock -> PressureBlock -> Float
pressureDot (PressureBlock _ a) (PressureBlock _ b) =
  VU.sum (VU.zipWith (*) a b)

pressureMix :: Float -> PressureBlock -> Float -> PressureBlock -> PressureBlock
pressureMix !wa (PressureBlock rows a) !wb (PressureBlock _ b) =
  PressureBlock rows (VU.zipWith (\x y -> wa * x + wb * y) a b)

pressureMaxAbsDelta :: PressureBlock -> PressureBlock -> Float
pressureMaxAbsDelta (PressureBlock rows a) (PressureBlock _ b) =
  blockMaxAbsDelta (rows * 8) a b

pressure8FromChannelsRow :: Channels -> Int -> Pressure8
pressure8FromChannelsRow !ch !r =
  Pressure8
    (channelsAt ch r 0)
    (channelsAt ch r 1)
    (channelsAt ch r 2)
    (channelsAt ch r 3)
    (channelsAt ch r 4)
    (channelsAt ch r 5)
    (channelsAt ch r 6)
    (channelsAt ch r 7)
{-# INLINE pressure8FromChannelsRow #-}

applyMatrix8 :: VU.Vector Double -> Pressure8 -> Pressure8
applyMatrix8 !m (Pressure8 x0 x1 x2 x3 x4 x5 x6 x7) =
  let c !ix = realToFrac (VU.unsafeIndex m ix) :: Float
      row !r =
        c (r * 8 + 0) * x0 + c (r * 8 + 1) * x1 +
        c (r * 8 + 2) * x2 + c (r * 8 + 3) * x3 +
        c (r * 8 + 4) * x4 + c (r * 8 + 5) * x5 +
        c (r * 8 + 6) * x6 + c (r * 8 + 7) * x7
  in Pressure8 (row 0) (row 1) (row 2) (row 3) (row 4) (row 5) (row 6) (row 7)
{-# INLINE applyMatrix8 #-}

pressureFaceClimate :: PressureBlock -> Int -> ClimateVec
pressureFaceClimate !press !i =
  let axisValue !a = realToFrac (pressureAt press i a)
  in ClimateVec (axisValue 0) (axisValue 1) (axisValue 2) (axisValue 3) (axisValue 4) (axisValue 5) (axisValue 6) (axisValue 7)
{-# INLINE pressureFaceClimate #-}
