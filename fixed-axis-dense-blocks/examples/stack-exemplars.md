# Transferable dense-block precedents

These patterns generalize beyond one solver, model, or numeric domain.

## Solver residual block

Use a fixed-axis block when residual, dot product, weighted mix, and max-delta all operate over the same homogeneous row shape.

Transfer rule: residual computation, convergence measurement, and update mixing should share one validated layout owner.

## Arena pair

Use immutable and mutable block pairs:

- immutable input snapshot;
- scoped mutable workspace;
- freeze boundary after the hot loop.

Transfer rule: the arena is an interpreter boundary. Do not let mutable storage become public semantics.

## Axis descriptor bridge

Use a closed axis descriptor list when UI, CSV, debug output, or metrics need names.

Transfer rule: descriptors are projections from the closed axis universe. They must not control storage length at runtime.

## Frontier iteration

Use selected-row kernels when only a dirty frontier changes.

Transfer rule: selected kernels need the same algebraic tests as whole-block kernels plus isolation tests for untouched rows.

## Dense-to-domain projection

Use row projection functions when the rest of the system speaks domain values.

Transfer rule: dense layout is private. Public APIs should expose row values, summaries, or typed projections, not offsets.
