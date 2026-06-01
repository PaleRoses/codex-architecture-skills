# Stack Exemplars

These are broader dense-block precedents worth stealing for `fixed-axis-dense-blocks` when the immediate row/block example is too local. The code snippets live in `../code-examples/source-corpus-snippets.md`.

## Selection rule

Keep an exemplar only if it shows at least one of these moves:

- bridge a modal or compressed solver state back to dense fixed-axis materialization;
- factor shared dense kernels below multiple concrete block types;
- seal mutable frontiers and dirty tracking inside arena ownership;
- keep dense materialization as a derived read model rather than a second authority.

## 1. `CoupledArena`: modal state and dense block materialization can share one owner

Code examples:

- `../code-examples/source-corpus-snippets.md#modal-arena-with-dense-materialization`

Structural lesson:

- a fixed-axis dense block is sometimes the public projection of a richer internal solver state;
- snapshotting, retargeting, dirty-delta computation, and dense materialization all belong to one arena boundary;
- the dense block becomes a derived view, not a rival mutable owner.

Reusable shape:

```haskell
data CoupledArena s = CoupledArena
  { caPlan      :: !CoupledPlan
  , caXBufs     :: !(MVector s Float)
  , caPrevXBufs :: !(MVector s Float)
  }

snapshotCoupledArena :: CoupledArena s -> ST s ()
materializeCoupledArena :: CoupledArena s -> Int -> Vector Int -> ST s Block
coupledArenaDeltaWithDirty :: CoupledArena s -> Int -> Vector Int -> Float -> (Int -> ST s ()) -> ST s Float
```

Steal when hot solver state lives in a transformed basis but callers still need a dense fixed-axis block at the boundary.

Rejects: duplicating modal buffers and dense blocks as semantically independent mutable owners.

## 2. `ReliefBlock` and `DenseBlock`: shared kernels belong below the domain block type

Code examples:

- `../code-examples/source-corpus-snippets.md#dense-numeric-kernels-factored-below-domain-blocks`

Structural lesson:

- fixed-axis block design improves when common numeric kernels are factored into a reusable dense core;
- the scalar one-axis case and the eight-axis case can share residual, dot, mix, and max-delta machinery without sharing semantics;
- this makes concrete blocks feel like lawful specializations rather than bespoke blobs.

Reusable shape:

```haskell
data ReliefBlock = ReliefBlock
  { rbRows :: !Int
  , rbData :: !(Vector Double)
  }

blockResidualInto :: Int -> MVector s a -> MVector s a -> MVector s a -> ST s ()
blockDotM :: Int -> MVector s a -> MVector s a -> ST s a
blockMixInto :: a -> Int -> MVector s a -> a -> MVector s a -> MVector s a -> ST s ()
```

Steal when multiple dense block types differ in semantic row shape but share the same kernel algebra.

Rejects: copying the same residual and max-delta loops into every block module.

## 3. `HydrologyArena` and `SedimentArena`: dirty-frontier mutation must stay sealed

Code examples:

- `../code-examples/source-corpus-snippets.md#solver-arena-sealed-around-dirty-frontiers`

Structural lesson:

- once a solver needs dirty faces, active pairs, basin frontiers, or lake carry state, those structures should live inside an arena, not leak into the public block API;
- mutable support buffers are acceptable when they are ownership-scoped and frozen results remain authoritative;
- frontier construction is part of the solver boundary, not an afterthought bolted onto a dense block.

Reusable shape:

```haskell
data Arena s = Arena
  { current    :: !(MBlock s)
  , previous   :: !(MBlock s)
  , dirtySeen  :: !(MVector s Word8)
  , dirtyBuf   :: !(MVector s Int)
  , activeSeen :: !(MVector s Word8)
  , activeBuf  :: !(MVector s Int)
  }
```

Steal when fixed-axis blocks participate in iterative frontier solvers with localized work reuse.

Rejects: public immutable block types that secretly require callers to manage dirty buffers and frontier sets themselves.

## Final review questions

A proposed exemplar is strong enough only if it helps answer:

```text
What is the true owner: raw block, transformed arena, or both in sequence?
Which kernels are shared beneath concrete block types?
Where do dirty-frontier and snapshot semantics live?
Which dense projection is derived rather than co-authoritative?
```
