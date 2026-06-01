---
name: structured-registry-designer
description: Design or review registries as lawful indexed structures. Use for lookup tables, environments, scope resolvers, module tables, capability dictionaries, handler tables, plugin tables, descriptor registries, config override systems, distributed registries, local-to-global sections, functorial transport, presheaf restriction, sheaf gluing, fibred registries, or enriched registries. Do not use for a one-off local map with no semantic indexing behavior.
---

# Structured Registry Designer

Design registries by their indexing semantics, not by their storage container.

A registry is defined by its index space, arrows between indices, value family, movement along arrows, and consistency policy. Storage is an implementation detail.

## Required inputs

Collect the registry purpose, key space, whether keys are closed or open, whether keys have arrows, what arrows mean, which direction values move, whether local values must agree on overlaps, whether values vary by key, and whether morphisms between registered values carry cost, resource, trust, version, proof, or migration semantics.

If the registry has no semantic indexing behavior, use a plain validated map at the boundary and say so.

## Default workflow

1. Identify the index structure. Keys may be a closed finite universe, runtime IDs, scopes, modules, contexts, graph nodes, cells, covers, or objects of a category.
2. Identify arrows. Name movement between keys in domain language before category language: import, refine, restrict, include, specialize, transport, inherit, override, migrate, project, or glue. Never leave arrows named only as morphisms.
3. Choose direction. Forward value movement is functorial. Parent-to-child visibility, refinement, import visibility, and context narrowing are usually presheaf restriction.
4. Choose the weakest lawful structure. Use a total registry for discrete closed keys, a functor or presheaf when arrows matter, a sheaf when overlap agreement and gluing matter, a fibred registry when each key has its own value category, and enriched or indexed extensions only when morphisms carry first-class semantics.
5. Define construction errors. Unknown keys, duplicate registrations, missing objects, broken morphisms, non-functorial transport, failed restriction, overlap mismatch, ambiguous glue, and unsupported migration must be named.
6. Expose semantic queries. Use `lookup` only for total or discrete registries. Prefer `restrictTo`, `transportAlong`, `visibleAt`, `sectionAt`, `glueCover`, `fiberAt`, or `reindexAlong` when those are the real operations.
7. Add law tests. Test identity, composition, totality, deterministic visibility, no hidden fallback, and conflict reporting. For sheaves, test overlap agreement, gluing existence, and gluing uniqueness.

## Resource map

Read `references/contract.md` for the registry ladder and hard structures.
Read `references/examples.md` for compact pattern reminders.
Read `references/quality-gates.md` before approving a design.
Read `references/anti-patterns.md` when auditing an existing registry.

## Example map

Use `examples/good-request.md` for the minimal trigger shape.
Use `examples/public-haskell-exemplars.md` for transferable public precedents.
Use `examples/source-corpus-exemplars.md` for portable source-shaped exemplars.

## Output contract

Return a `Structured Registry Plan` with the selected structure, rejected alternatives, indexing category, arrow direction, value family, construction boundary, query surface, conflict policy, gluing policy, and tests.

## Gotchas

Parent fallback is usually not lookup. It is restriction along a scope relation.

A descriptor table over a closed witness universe is a total registry, not a runtime plugin map.

Distributed local registrations require an overlap story. If compatible locals must assemble into one global view, use sheaf semantics or explicitly say why uniqueness is not required.

## Final self-check

Confirm the design has one canonical index structure, domain-named arrows, explicit arrow direction, no raw string map as canonical semantics, no silent override chain, no global mutable singleton registry, no duplicate semantic owner, and no distributed gluing without overlap checks.
