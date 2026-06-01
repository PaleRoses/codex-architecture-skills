---
name: fixed-axis-dense-blocks
description: Design fixed-axis dense numeric block layouts for hot loops. Use when a model has a small fixed homogeneous axis family, strict row values, immutable and mutable block forms, column-major or row-major layout decisions, selected-row kernels, residual, dot, mix, and max-delta operations, solver arenas, vectorized scoring, or public projection from dense storage. Do not use for sparse, ragged, open-axis, or heterogeneous payloads.
---

# Fixed-Axis Dense Blocks

Design dense numeric storage for small fixed homogeneous axis families without tying the pattern to one domain such as pressure, climate, signal, or material state.

Use this when the axis count is fixed, the scalar type is homogeneous, row-level operations are hot, and the layout will sit behind a domain-specific public API.

## Required inputs

Collect the axis family, axis count, scalar type, row count, public row value, immutable block, mutable block, canonical memory layout, row read or write operations, selected-row kernels, numeric kernels, ownership boundary, and projection format.

If the axis set is open, sparse, ragged, or heterogeneous, do not use this pattern. Use sparse vectors, maps, product records, or typed aggregates instead.

## Default workflow

1. Prove the axis family is small, fixed, and homogeneous.
2. Define a strict row value such as `Signal4`, `Material6`, or `Field8`. Keep it domain-named but structurally fixed-width.
3. Define one immutable block and one scoped mutable block. The public API returns immutable values; hot loops may use scoped mutation.
4. Choose one canonical layout and document it. Column-major `axis * rows + row` works well for axis-wise sweeps; row-major works for row-wise kernels. Do not support both as semantic peers.
5. Implement row read, write, and add, scalar axis access, whole-block copy, selected-row copy and fill, freeze, thaw, and clone-freeze.
6. Implement numeric kernels: residual, dot, weighted mix, max absolute delta, selected-row max absolute delta, and optional scaling or small matrix application.
7. Validate shape at construction boundaries. Internal unsafe indexing is acceptable only after row count and storage length invariants are established.
8. Project dense rows back to public domain values at the boundary.
9. Add tests for layout indexing, read or write round trips, selected-row isolation, kernel algebra, shape mismatch, NaN policy, and public projection.

## Resource map

Read `references/contract.md` for the hard structure.
Read `references/examples.md` for compact pattern reminders.
Read `references/quality-gates.md` before approving a design.
Read `references/anti-patterns.md` when reviewing an existing block layout.
Read `code-examples/source-corpus-snippets.md` only when the exemplar prose needs concrete Haskell snippets.

## Example map

Use `examples/good-request.md` for the minimal trigger shape.
Use `examples/stack-exemplars.md` for stronger repo-native precedents elsewhere in the Melusine/Moonlight stack.
Use `examples/source-corpus-exemplars.md` for the repo-local canonical structures.

## Output contract

Return a `Fixed-Axis Dense Block Plan` with the axis family, scalar type, row value, block types, layout formula, operations, ownership boundary, numeric kernels, projection boundary, and tests.

## Gotchas

The row value is not the block. The row value is a strict product for one row; the block is the contiguous storage for all rows.

The mutable block must not escape its scope. Do not create duplicate ownership between public immutable state and internal mutable arenas.

A layout switch is a migration, not a flag. Pick one canonical layout for the skill target.

## Final self-check

Confirm the axis count is fixed, storage length is `rows * axes`, layout has one formula, selected-row kernels cannot modify other rows, unsafe indexing is hidden behind validated construction, and public projection does not expose the storage layout as semantics.
