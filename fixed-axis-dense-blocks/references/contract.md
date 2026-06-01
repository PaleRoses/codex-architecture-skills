# Contract: Fixed-Axis Dense Block Layout

## Applicability

Use when all are true:

- Axis count is small and fixed at compile time or construction time.
- Every axis uses the same scalar type.
- Row operations or axis sweeps are hot enough to justify dense storage.
- A domain-specific row value is useful at API boundaries.
- Shape can be validated before unsafe indexing begins.

Do not use for sparse, ragged, heterogeneous, or user-extensible axis sets.

## Hard structure

```haskell
-- Domain-named strict row value.
data FieldN = FieldN !Float !Float !Float !Float

-- Immutable public block.
data FieldBlock = FieldBlock
  { blockRows :: !Int
  , blockData :: !(Vector Float)
  }

-- Scoped mutable block.
data MFieldBlock s = MFieldBlock
  { mblockRows :: !Int
  , mblockData :: !(MVector s Float)
  }
```

## Layout formula

Pick one canonical formula and document it.

Column-major:

```haskell
ix rows axis row = axis * rows + row
```

Row-major:

```haskell
ix axes row axis = row * axes + axis
```

Column-major is usually stronger for axis-wise sweeps and selected-row operations across all axes. Row-major may be stronger for contiguous row kernels. Do not expose a switch unless the layout itself is the object being optimized.

## Required operations

- `replicateBlock rows scalar`
- `generateBlock rows (row -> FieldN)`
- `newMutable rows scalar`
- `thawBlock`, `freezeBlock`, `freezeClone`
- `readRow`, `writeRow`, `addRow`
- `readAxis`, `writeAxis`
- `copyBlock`, `copyRows`, `fillRows`
- `residualInto out new old`
- `dotMutable a b`
- `mixInto weightA out weightB a b`
- `maxAbsDelta a b`
- `maxAbsDeltaRows rows a b`
- Optional `diagMul`, `applySmallMatrix`, `projectPublic`

## Required invariants

- `length data == rows * axisCount`.
- Row index must be in `[0, rows)`.
- Axis index must be in `[0, axisCount)`.
- Mutable block row count must match source block row count before copy/mix kernels.
- Selected-row operations modify only selected rows.
- Public projection never exposes the internal layout as domain semantics.

## Required tests

- Generated block readback matches the generator.
- Write/read round trip for every axis.
- Copy and selected-row copy preserve unselected rows.
- Residual equals `new - old`.
- Dot product matches reference implementation.
- Mix matches reference implementation.
- Max delta matches reference implementation.
- Invalid shape is rejected before unsafe kernels run.
