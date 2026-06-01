---
name: hkd-indexed-data
description: Design higher-kinded data records, indexed products, and witness-indexed heterogeneous families. Use when creating or reviewing HKD records, records parameterized by Identity/Maybe/Const/Vector/Validation carriers, Column f a schemas, closed witness-indexed products, CoveringProduct-style structures, ChannelVec-style families, type-family payloads by index, sparse-to-total authoring views, or phase-indexed record variants. Do not use for ordinary recursive syntax functors, tagless effect carriers, open plugin registries, one-off records with no repeated shape, or homogeneous maps.
---

# HKD Indexed Data

Design one logical data shape whose carriers, witnesses, and indexed payload families produce raw, partial, validated, compiled, projected, dense, or diagnostic views without duplicating semantic ownership.

This skill sits between `closed-vocabulary-constructor` and `structured-registry-designer`: use it when the index universe is closed enough to have a total product, but the payload varies by field, phase, or witness.

## Required inputs

Collect the logical field or index universe, whether it is closed, the intended views or phases, the carrier family, whether payloads are homogeneous or heterogeneous, the witness type, the per-index payload family, partial authoring policy, constructor/decoder boundary, public query surface, derived dense or sparse views, and law tests.

If the universe is open-ended, design a structured registry instead. If there is only one concrete record and no repeated view shape, keep the plain record. Do not introduce HKD just to look learned. The type system is not a costume party.

## Default workflow

1. Identify the owner of the shape. Name the one schema, field universe, or witness family that owns the structure.
2. Choose the indexed form.
   - Use `Record carrier` when the same named fields appear across carriers such as `Identity`, `Maybe`, `Const`, validation, vectors, or codecs.
   - Use `Product witness payload` or `CoveringProduct witness payload` when a closed witness universe indexes a heterogeneous payload family.
   - Use a type family such as `Payload ix` or `Column carrier a` when each index or carrier determines the field representation.
3. Name carrier semantics. `f` must mean raw, optional, validated, encoded, batched, vectorized, diagnostic, or compiled. A nameless polymorphic parameter is just fog with kind signatures.
4. Keep one canonical universe. Derive partial, dense, encoded, projected, and diagnostic views from the same owner. Do not create parallel `RawX`, `ValidatedX`, and `CompiledX` records unless they are generated views or intentionally separate domains.
5. Carry field evidence when errors or partial updates need attribution. Use field witnesses, typed selectors, or existential witness wrappers; do not smuggle field identity through strings.
6. Provide total witness operations. Define `tabulate`, `index`, `mapWithWitness`, `zipWithWitness`, `traverseWithWitness`, `foldMapWithWitness`, and `replace` when the universe is closed and total.
7. Resolve partial authoring once. Use `Maybe`, `Alt Maybe`, validation accumulators, or sparse fragments as carriers, then construct one total canonical product at the boundary.
8. Preserve per-index type safety. Existential wrappers may hide the index only with a witness that recovers the correct payload type. Never recover through strings or casts.
9. Treat dense storage as derived. Dense vectors, arrays, and row encodings are execution views; symbolic witnesses and constructors remain authoritative.
10. Add named failures. Missing fields, duplicate witnesses, unknown wire IDs, carrier mismatch, unresolved partial values, invalid payloads, and projection gaps should be typed errors.
11. Test the laws. Totality, round trips, carrier traversal, per-index isolation, sparse-to-total resolution, codec stability, dense projection, and adding a new witness should all be tested.

## Resource map

Read `references/contract.md` for the hard structures.
Read `references/examples.md` for compact pattern reminders.
Read `references/quality-gates.md` before approving a design.
Read `references/anti-patterns.md` when reviewing an existing implementation.

## Example map

Use `examples/good-request.md` for the minimal trigger shape.
Use `examples/source-corpus-exemplars.md` for repo-local canonical structures.
Use `examples/stack-exemplars.md` for transferable shapes from nearby packages.

## Output contract

Return an `HKD Indexed Data Plan` with the selected indexed form, rejected alternatives, owner universe, witness design, carrier semantics, payload family, constructor boundary, partial/full view policy, projection policy, public operations, typed failures, and tests.

## Gotchas

An HKD record is useful only when carriers remove duplicated record definitions. If every field needs unrelated behavior, use ordinary domain records and explicit functions.

A witness-indexed product is not a plugin registry. Closed witnesses get total products; open plugins get registries.

A dense vector is not the canonical schema. It is a projection with a proof obligation.

Generic HKT usage is not enough. Recursion schemes, tagless-final effects, and syntax functors are excellent machinery, but they are not this skill unless the task is about one repeated record/product shape across carriers or witnesses.

## Final self-check

Confirm there is one owner of the shape, a closed index or field universe, named carrier semantics, total witness operations where appropriate, one constructor boundary, no stringly recovery path, no duplicate phase records pretending to be independent truth, no dense view as canonical semantics, and law tests that fail when the shape grows without updating derived views.
