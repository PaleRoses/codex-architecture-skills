# Source Corpus Exemplars

These are the local structures that matter for `fixed-axis-dense-blocks`. They are the nearby canonical target shapes in this repository.

## 1. `PressureBlock`: fixed-axis dense storage with a real layout contract

Source file:

- `files/compiler/engine/melusine-world-schober/growth/src/Melusine/World/Growth/Solver/PrimordialField/Pressure/Block.hs`

Structural lesson:

- the reusable pattern is not “pressure”; it is a strict fixed-width row plus dense block storage;
- layout is chosen once and then fed to real kernels;
- public row semantics and internal storage order are distinct layers.

Reusable shape:

```haskell
data Row8 = Row8 !Float !Float !Float !Float !Float !Float !Float !Float

blockIx :: Int -> Int -> Int -> Int
blockIx rows axis row = axis * rows + row
```

Transfer rule: use this when a small fixed homogeneous axis family dominates hot numeric work.

Rejects: domain-specific names baked into the reusable storage abstraction.

## 2. selected-row dirty-copy kernels: incremental work should not sweep the whole block

Source file:

- `files/compiler/engine/melusine-world-schober/growth/src/Melusine/World/Growth/Solver/PrimordialField/Engine.hs`

Relevant functions include the dirty-copy paths around `copyPressureAllWithDirty` and `copyPressureFacesWithDirty`.

Structural lesson:

- dense block quality includes update schedule quality;
- selected-row kernels are first-class when dirty-frontier iteration matters;
- convergence measurement can share the copy boundary instead of forcing a second pass.

Transfer rule: add selected-row operations when solver work is sparse in space but dense in per-row math.

Rejects: full-block copies for every local update.

## 3. `Channels`: know when the fixed-axis skill should be rejected

Source file:

- `files/compiler/engine/melusine-world-schober/growth/src/Melusine/World/Growth/Internal/Channels.hs`

Structural lesson:

- dynamic row or column counts belong in a generic dense matrix structure;
- not every dense numeric layout should be squeezed into a fixed-width row product;
- the rejection boundary is part of the skill quality bar.

Transfer rule: if the axis family is open or runtime-variable, stop and use the generic channel matrix instead.

Rejects: pretending a dynamic channel table is a fixed-axis block.

## 4. solver arenas: mutation becomes acceptable only when ownership is sealed

Source files:

- `files/compiler/engine/melusine-world-schober/growth/src/Melusine/World/Growth/Solver/PrimordialField/Engine.hs`
- `files/compiler/engine/melusine-world-schober/growth/src/Melusine/World/Growth/Solver/Solstice/Hydrology/Arena.hs`
- `files/compiler/engine/melusine-world-schober/growth/src/Melusine/World/Growth/Solver/Solstice/Engine/Types.hs`

Structural lesson:

- mutable arenas are acceptable when they are strictly scoped and the frozen public result is the only exported owner;
- scratch buffers, dirty sets, and work fronts belong with the arena, not with the public row API.

Reusable shape:

```haskell
data Arena s = Arena
  { current  :: !(MBlock axis s)
  , next     :: !(MBlock axis s)
  , residual :: !(MBlock axis s)
  , dirty    :: !(MVector s Int)
  }
```

Transfer rule: pair dense blocks with arena ownership when iterative solvers need repeated in-place kernels.

Rejects: mutable state leaking through the public immutable boundary.

## Final review questions

Use the local corpus correctly only if you can answer:

```text
Which module owns the canonical layout formula?
Where are selected-row kernels explicit rather than improvised?
Where does the generic dynamic matrix take over from the fixed-axis block?
Which mutable structures are sealed inside an arena boundary?
```
