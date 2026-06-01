# Anti-patterns

- Duplicated `RawX`, `ValidatedX`, and `CompiledX` records with manually synchronized fields.
- `Map String Dynamic` as the recovery path for a closed heterogeneous product.
- A polymorphic `f` parameter whose carriers are never named.
- HKD for a one-off record that has no alternate views.
- Open plugin IDs represented as a closed witness universe.
- Dense vector offsets maintained separately from the witness family.
- Existential payloads with no eliminator, no witness, or a runtime cast.
- String tags, `show`, or constructor names used as semantic dispatch.
- Per-field typeclasses that obscure the single owner of the product shape.
- Compatibility wrappers that keep old phase records alive beside the HKD owner.
- A `Record f` whose `f` has no domain meaning beyond "I wanted an HKT".
- Sparse fragments allowed to leak into consumers after the constructor boundary.
- Adding a new field without a test that forces codecs, labels, partial resolution, and projections to move with it.
