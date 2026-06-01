# Portable source-shaped exemplars

Use these as dense-block transfer patterns for real code.

## Four-axis field block

```haskell
data Field4 = Field4
  { fieldHeat  :: !Double
  , fieldWater :: !Double
  , fieldWind  :: !Double
  , fieldMineral :: !Double
  }

newtype FieldBlock = FieldBlock Vector
```

Shape: strict public row value plus contiguous storage for many rows.

Transfer rule: `Field4` is not the block. It is the public row projection. The block owns layout and row count.

## Scoped mutable arena

```haskell
newtype MutableFieldBlock s = MutableFieldBlock (MutableVector s Double)
```

Shape: mutable hot-loop storage whose scope parameter prevents escape.

Transfer rule: mutation is an interpreter detail. Public state freezes back into immutable blocks or row values.

## Selected-row kernel

```haskell
maxAbsDeltaSelected :: RowSet -> FieldBlock -> FieldBlock -> Double
```

Shape: frontier-aware numeric kernel that touches only chosen rows.

Transfer rule: selected-row operations must prove isolation. Tests should fail if an unselected row changes or contributes.

## Canonical layout formula

```haskell
index axis row = axis * rowCount + row
```

Shape: column-major layout for axis-wise sweeps.

Transfer rule: choose one layout formula and make every read, write, copy, and kernel share it. A layout switch is a migration, not a runtime flag.

## Boundary projection

```haskell
readRow :: FieldBlock -> RowId -> Field4
writeRow :: MutableFieldBlock s -> RowId -> Field4 -> ST s ()
```

Shape: dense internal representation with domain-shaped public values.

Transfer rule: callers consume domain rows, not storage offsets.
