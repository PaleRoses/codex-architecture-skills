# Source Corpus Exemplars

These are the local structures that matter for `structured-registry-designer`. They show the registry ladder in actual repo form rather than abstract sermonizing.

## 1. `ChannelVec` and descriptor tables: closed universes deserve total registries

Source files:

- `files/compiler/engine/melusine-algebra/src/Melusine/Algebra/Pure/ChannelVec.hs`
- `files/compiler/engine/melusine-algebra/src/Melusine/Algebra/Pure/ChannelDescriptor.hs`

Structural lesson:

- once the witness universe is closed, the registry should be a product over that universe;
- lookup by witness is total and type-recovering;
- string-keyed descriptor maps are the weaker lie.

Reusable shape:

```haskell
data SegmentWitness segment where
  ConfigWitness :: SegmentWitness 'Config
  CacheWitness  :: SegmentWitness 'Cache

entryAt :: SegmentWitness segment -> SegmentVec f -> f segment
```

Transfer rule: use total products for closed registries and make missing entries impossible after construction.

Rejects: partial lookup over a declared finite universe.

## 2. `ChannelAggregate` and `LawfulChannel`: registry entries may carry algebra, not only data

Source files:

- `files/compiler/engine/melusine-algebra/src/Melusine/Algebra/Pure/ChannelAggregate.hs`
- `files/compiler/engine/melusine-algebra/src/Melusine/Algebra/Pure/LawfulChannel.hs`

Structural lesson:

- the registry entry is often the law surface for that segment;
- equality, composition, application, and replay semantics belong with the descriptor;
- a registry that forgets the governing algebra is only half a registry.

Transfer rule: treat law-bearing descriptors as first-class registry payloads when callers need semantics, not only storage.

Rejects: tables that recover a payload but not the operations or laws that make it usable.

## 3. declared site manifests: local-to-global structure should be declared explicitly

Source files:

- `files/compiler/engine/melusine-domain/src/Melusine/Domain/Architecture/Manifest.hs`
- `files/compiler/foundation/moonlight-category/src-core/Moonlight/Category/Pure/Site/Manifest.hs`
- `files/compiler/foundation/moonlight-category/src-core/Moonlight/Category/Pure/Site/Compile.hs`

Structural lesson:

- when the registry has covers, overlaps, and admissible locality, the site is part of the design;
- gluing obligations should not be reconstructed from ad hoc traversal order later;
- the cover structure deserves its own owner.

Transfer rule: elevate site, cover, and locality declarations into typed data when overlap and descent are real semantics.

Rejects: distributed registries whose notion of overlap exists only in prose.

## 4. sheaf and transport layers: arrows must do real work

Source files:

- `files/compiler/engine/melusine-sheaf/src/Melusine/Sheaf/Site/Stalk.hs`
- `files/compiler/engine/melusine-sheaf/src/Melusine/Sheaf/Section/Potential.hs`
- `files/compiler/foundation/moonlight-category/src-core/Moonlight/Category/Pure/GeometricFunctor.hs`
- `files/compiler/engine/melusine-domain/src/Melusine/Domain/Gluing/ComposeDecorated.hs`

Structural lesson:

- some registries are not lookup surfaces at all; they are restriction, transport, and composition surfaces;
- once arrows carry semantics, the direction and law story must be explicit;
- local sections, transport, and gluing are not implementation flourishes.

Reusable shape:

```haskell
data Presheaf k section = Presheaf
  { sectionAt :: forall a. Object k a => section a
  , restrict  :: forall a b. k a b -> section b -> section a
  }
```

Transfer rule: model movement along arrows directly when visibility, refinement, or transport is the real operation.

Rejects: fallback chains and copy-by-convention across related keys.

## Final review questions

Use the local corpus correctly only if you can answer:

```text
Is this registry total, sparse, presheaf-like, sheaf-like, or functorial?
Where is the witness that recovers the payload type?
Where are overlap and transport laws declared instead of implied?
Which module owns the index structure itself?
```
