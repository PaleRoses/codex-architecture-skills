# Examples

## Raw/canonical/encoded record

Use an HKD record when the same fields appear as optional raw input, canonical values, and encoded columns.

Hard choices:

- `Member f` is the owner.
- `Column f a` defines each carrier view.
- validation resolves `Member Maybe` into `Member Identity` once.
- codecs and diagnostics derive from the owner.
- field witnesses are introduced if errors, labels, sparse patches, or per-field updates need attribution.

## Channel-indexed aggregate

Use a witness-indexed product when each closed channel has a different state and delta type.

Hard choices:

- `ChannelWitness ch` recovers the index.
- type families define `ChannelState ch` and `ChannelDelta ch`.
- `ChannelVec f` is total over the closed universe.
- existential deltas carry their witness.
- registry operations are indexed by the witness, not by string dispatch.

## Sparse authoring over total shape

Use a partial carrier for authoring fragments.

Hard choices:

- missing values are errors or declared defaults at construction.
- sparse fragments do not define a second schema.
- resolution produces one canonical total product.
- consumers never receive a partially resolved product unless the domain explicitly models partiality.

## Generic product abstraction

Use a `CoveringProduct w f`-style abstraction when several closed witness families need the same total-product operations.

Hard choices:

- `index` and `tabulate` are the primitive laws.
- `foldMap` requires a `CoveringFamily` enumeration.
- `replace` requires typed witness equality.
- subset restriction is a typed witness embedding, not a key filter.

## Reject: ordinary syntax functor

`Pattern f`, `Fix f`, and `ENode f` are good higher-kinded term machinery, but not this skill's main target. Use recursion/e-graph reasoning instead unless the task is specifically about carrier-indexed records or witness-indexed products.

## Reject: open plugin table

If users can register arbitrary new members at runtime, use a structured registry. Do not squeeze an open plugin universe into a fake closed product.
