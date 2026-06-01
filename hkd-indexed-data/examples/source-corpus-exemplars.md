# Source Corpus Exemplars

These are the local structures that matter for `hkd-indexed-data`. They are nearby canonical grip points, not decorative links.

## 1. `ChannelVec`: closed heterogeneous product by witness

Source files:

- `files/compiler/engine/melusine-algebra/src/Melusine/Algebra/Pure/ChannelVec.hs`
- `files/compiler/engine/melusine-algebra/src/Melusine/Algebra/Pure/ChannelDelta.hs`

Structural lesson:

- `CanonicalChannel` is the closed universe;
- `ChannelWitness channel` recovers the index;
- `ChannelState channel` and `ChannelDeltaPayload channel` attach heterogeneous payloads;
- `ChannelVec f` gives total storage over the family;
- `tabulateChannelVec`, `indexChannelVec`, `traverseChannelVecWithWitness`, and `replaceChannelVec` are the essential surface.
- `ChannelConstraintBundle constraint` is how per-channel constraints are carried without a stringly side table.
- `SomeChannelWitness` and `SomeChannelDelta` are acceptable existential shells because the typed witness is still recoverable.

Transfer rule: use this when a closed family has different payload types per member but still needs total product operations.

Rejects: `Map ChannelId SomePayload` as canonical semantics.

Review grip:

```text
Can every channel be indexed without failure?
Does every existential value still carry the channel witness?
Does adding a channel force ChannelState, ChannelDeltaPayload, tabulation, indexing, registry, and tests to move together?
```

## 2. `CoveringProduct`: abstract dependent product over witnesses

Source file:

- `files/compiler/foundation/moonlight-category/src-core/Moonlight/Category/Pure/CoveringProduct.hs`

Structural lesson:

- `CoveringProduct w f` is the generic form of a total witness-indexed product;
- `tabulateCoveringProduct` and `indexCoveringProduct` are the law-bearing core;
- `restrictCoveringProduct` handles typed subset projection without a runtime key filter;
- targeted replacement requires typed witness equality;
- `foldMapCoveringProductWithWitness` depends on `CoveringFamily w`, so iteration is derived from the witness universe, not hand-maintained map keys.

Transfer rule: use this when the product pattern should be abstract over the witness universe.

Rejects: rebuilding a separate product type for every subset projection.

Review grip:

```text
Is the product just a function from witness to payload?
Is subset projection a typed witness embedding?
Is replacement guarded by witness equality rather than by text labels?
```

## 3. `Column f a`: HKD record view selection

Source files:

- `files/compiler/foundation/moonlight-flow/model/src/Moonlight/Flow/Model/Family.hs`
- `files/compiler/foundation/moonlight-flow/test-support/test/Moonlight/Flow/Runtime/PublicApiSpec.hs`

Structural lesson:

- `Column f a` lets one record schema describe several representations;
- `Column Identity a` is the canonical scalar value;
- `Column VU.Vector a` is the columnar/vectorized representation;
- `Column (Const x) a` is the metadata/header representation;
- `Member Identity` is the canonical row value in the test support;
- `SAtomFamily fam` decodes runtime rows into `fam Identity` through one schema owner;
- row-width mismatch is a typed decode error, not a failed pattern match pretending to be impossible.

Transfer rule: use this when raw, canonical, encoded, columnar, or diagnostic views share the same named fields.

Rejects: manually synchronized row/header/value records.

Review grip:

```text
Which single record owns the fields?
What does each carrier mean operationally?
Where does decoding cross from runtime row into fam Identity?
Which errors are named at the decode boundary?
```

## 4. Typed modality registries: dependent keys are the adjacent boundary

Source files:

- `files/compiler/foundation/moonlight-sheaf/obstruction/src/Moonlight/Sheaf/Obstruction/Cohomological/Modality.hs`
- `files/compiler/foundation/moonlight-sheaf/obstruction/src/Moonlight/Sheaf/Obstruction/Cohomological/Substrate.hs`

Structural lesson:

- `ModalityRegistry key` uses dependent keys to carry the payload type selected by each key;
- lowering to relation/projection form is a derived view;
- gaps and unsupported anchors are typed failures, not missing map entries.
- `DMap`/dependent sums are appropriate only when the key preserves enough type evidence to safely recover the value.

Transfer rule: use typed keys and dependent maps when the universe is registry-shaped rather than total-product-shaped, but the value family still must remain type-indexed.

Rejects: untyped capability dictionaries.

Review grip:

```text
Does the key select the value type?
Is lowering/projection downstream of the typed owner?
Are missing references and unsupported anchors returned as typed gaps?
```

## 5. Cohomological substrate aliases: phase-specific views should stay derived

Source file:

- `files/compiler/foundation/moonlight-sheaf/obstruction/src/Moonlight/Sheaf/Obstruction/Cohomological/Substrate.hs`

Structural lesson:

- aliases such as `SubstrateRegion`, `SubstrateExactMatch`, `SubstrateWitness`, and `SubstrateRegionSummary` fix a substrate-indexed family into concrete execution views;
- `CohomologicalSupportAlgebra request key region evidence result ref gap tag` keeps the request/key/region/evidence/result/gap/tag families explicit instead of collapsing them into an untyped environment;
- support, exact-match, witness, cache, and projection views are downstream of the substrate family.

Transfer rule: use family-indexed aliases when many phase-specific views are all consequences of one substrate parameter.

Rejects: copying the same record for every phase because the payload type changed.

## Final review questions

```text
What owns the product shape?
Is the universe closed enough for total indexing?
Which carrier or witness recovers each payload type?
Which views are derived rather than authoritative?
Which operation proves the product is total?
Which test fails when the product gains a new member?
```
