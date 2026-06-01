# Fixed-Axis Dense Blocks Code Examples

These snippets are the code corpus for `fixed-axis-dense-blocks`. They live inside the skill folder so the dense-block examples are portable without a fake repository tree.

## Strict row value and dense block owner

```haskell
type Pressure8 :: Type
data Pressure8 = Pressure8
  !Float !Float !Float !Float !Float !Float !Float !Float

add8 :: Pressure8 -> Pressure8 -> Pressure8
add8 (Pressure8 a0 a1 a2 a3 a4 a5 a6 a7)
     (Pressure8 b0 b1 b2 b3 b4 b5 b6 b7) =
  Pressure8
    (a0 + b0) (a1 + b1) (a2 + b2) (a3 + b3)
    (a4 + b4) (a5 + b5) (a6 + b6) (a7 + b7)

maxAbs8 :: Pressure8 -> Float
maxAbs8 (Pressure8 a0 a1 a2 a3 a4 a5 a6 a7) =
  max (abs a0)
    (max (abs a1)
      (max (abs a2)
        (max (abs a3)
          (max (abs a4)
            (max (abs a5)
              (max (abs a6) (abs a7)))))))
```

```haskell
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

pressureIx :: Int -> Int -> Int -> Int
pressureIx !rows !a !i = a * rows + i
```

## Boundary projection and scoped mutation

```haskell
freezePressure :: MPressureBlock s -> ST s PressureBlock
freezePressure (MPressureBlock rows dat) = do
  frozen <- VU.unsafeFreeze dat
  pure (PressureBlock rows frozen)

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
```

```haskell
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
```

## Selected-row kernels

```haskell
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
```

```haskell
copyPressureRowDelta ::
  Int -> VUM.MVector s Float -> VUM.MVector s Float -> Int -> ST s Float
copyPressureRowDelta !rows !dst !src !i = do
  let !ix0 = i
      !ix1 = rows + i
      !ix2 = ix1 + rows
      !ix3 = ix2 + rows
      !ix4 = ix3 + rows
      !ix5 = ix4 + rows
      !ix6 = ix5 + rows
      !ix7 = ix6 + rows
  -- read old/new row, write new row, return max absolute row delta
```

```haskell
copyPressureFacesWithDirty ::
  SharedArena s -> STRef s Int -> Double ->
  VU.Vector Int -> MPressureBlock s -> MPressureBlock s -> ST s Double
copyPressureFacesWithDirty !shared !dirtyRef !eps !faces !(MPressureBlock rows dst) !(MPressureBlock _ src) = do
  let !epsF = realToFrac eps :: Float
      !count = VU.length faces
      faceLoop !ix !best
        | ix == count = pure (realToFrac best)
        | otherwise = do
            let !i = VU.unsafeIndex faces ix
            d <- copyPressureRowDelta rows dst src i
            when (d > epsF) (markDirtyFace shared dirtyRef i)
            faceLoop (ix + 1) (max best d)
  faceLoop 0 0.0
```

## Dense numeric kernels factored below domain blocks

```haskell
blockResidualInto ::
  (VUM.Unbox a, Num a) =>
  Int -> VUM.MVector s a -> VUM.MVector s a -> VUM.MVector s a -> ST s ()

blockDotM ::
  (VUM.Unbox a, Num a) =>
  Int -> VUM.MVector s a -> VUM.MVector s a -> ST s a

blockMixInto ::
  (VUM.Unbox a, Num a) =>
  a -> Int -> VUM.MVector s a -> a -> VUM.MVector s a -> VUM.MVector s a -> ST s ()

blockMaxAbsDeltaM ::
  (VUM.Unbox a, Num a, Ord a) =>
  Int -> VUM.MVector s a -> VUM.MVector s a -> ST s a
```

```haskell
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
```

## Rejection boundary: dynamic channels are not fixed-axis blocks

```haskell
type Channels :: Type
data Channels = Channels
  { chRows :: !Int
  , chCols :: !Int
  , chData :: !(VU.Vector Float)
  }

channelsEmpty :: Int -> Int -> Channels
channelsEmpty !rows !cols =
  Channels rows cols (VU.replicate (rows * cols) 0.0)

channelsAt :: Channels -> Int -> Int -> Float
channelsAt !ch !i !c =
  VU.unsafeIndex (chData ch) (idx2 i c (chCols ch))
```

## Modal arena with dense materialization

```haskell
type CoupledArena :: Type -> Type
data CoupledArena s = CoupledArena
  { caPlan :: !CoupledPlan
  , caXBufs :: !(VUM.MVector s Float)
  , caPrevXBufs :: !(VUM.MVector s Float)
  , caBBufs :: !(VUM.MVector s Float)
  , caWorkR :: !(MFloatVec s)
  , caWorkP :: !(MFloatVec s)
  , caWorkZ :: !(MFloatVec s)
  , caWorkAp :: !(MFloatVec s)
  }

snapshotCoupledArena :: CoupledArena s -> ST s ()
snapshotCoupledArena !arena =
  VUM.copy (caPrevXBufs arena) (caXBufs arena)
```

```haskell
materializeCoupledArena :: CoupledArena s -> Int -> VU.Vector Int -> ST s PressureBlock
materializeCoupledArena !arena !rows !activeFaces = do
  out <- newPressureMutable rows 0.0
  let !plan = caPlan arena
      !lo = cpClipLo plan
      !hi = cpClipHi plan
      !n = VU.length activeFaces
      faceLoop !ix
        | ix == n = pure ()
        | otherwise = do
            let !i = VU.unsafeIndex activeFaces ix
            p <- clipWith lo hi <$> reconstructPressure8 plan (caXBufs arena) rows i
            pressureWrite8 out i p
            faceLoop (ix + 1)
  faceLoop 0
  freezePressure out
```

```haskell
coupledArenaDeltaWithDirty ::
  CoupledArena s -> Int -> VU.Vector Int -> Float -> (Int -> ST s ()) -> ST s Float
coupledArenaDeltaWithDirty !arena !rows !activeFaces !eps !markDirty = do
  let !plan = caPlan arena
      !n = VU.length activeFaces
      faceLoop !ix !best
        | ix == n = pure best
        | otherwise = do
            let !i = VU.unsafeIndex activeFaces ix
            oldP <- reconstructPressure8 plan (caPrevXBufs arena) rows i
            newP <- reconstructPressure8 plan (caXBufs arena) rows i
            let !d = maxAbs8 (sub8 newP oldP)
            when (d > eps) (markDirty i)
            faceLoop (ix + 1) (max best d)
  faceLoop 0 0.0
```

## Solver arena sealed around dirty frontiers

```haskell
type HydrologyArena :: Type -> Type
data HydrologyArena s = HydrologyArena
  { haLakeCarry :: !(MReliefBlock s)
  , haPrevRelief :: !(MReliefBlock s)
  , haDirtySeen :: !(MWord8Vec s)
  , haDirtyBuf :: !(MIntVec s)
  , haBasinSeen :: !(MWord8Vec s)
  , haBasinBuf :: !(MIntVec s)
  , haActiveSeen :: !(MWord8Vec s)
  , haActiveBuf :: !(MIntVec s)
  , haPairSeen :: !(MWord8Vec s)
  , haPairBuf :: !(MIntVec s)
  }
```

```haskell
collectDirtyFaces ::
  Double -> HydrologyArena s -> ScalarField -> ST s (VU.Vector Int)
collectDirtyFaces !eps !arena !relief = do
  let !rows = VU.length relief
  countRef <- newSTRef 0
  let go !i
        | i == rows = pure ()
        | otherwise = do
            oldZ <- reliefReadM (haPrevRelief arena) i
            let !newZ = VU.unsafeIndex relief i
            when (abs (newZ - oldZ) > eps) $
              markInto (haDirtySeen arena) (haDirtyBuf arena) countRef i
            go (i + 1)
  go 0
  dirty <- freezeBuf (haDirtyBuf arena) countRef
  clearMarks (haDirtySeen arena) dirty
  pure dirty
```
