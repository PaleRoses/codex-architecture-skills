# Examples

## Signal4 block

Axes:

```text
Latency, Throughput, ErrorRate, Saturation
```

Use a strict `Signal4` row value and a `SignalBlock` for many rows. Column-major storage is useful when kernels sweep the same axis over all rows.

## Material6 block

Axes:

```text
Hardness, Porosity, Density, Conductivity, Roughness, Reflectance
```

Use a dense block when simulation kernels repeatedly compute residuals and mixes across all material axes.

## Field8 block

Axes:

```text
A0..A7
```

Use when a solver has exactly eight homogeneous fields and selected-row updates dominate runtime.

## Reject: sparse sensor map

A sparse sensor map with arbitrary sensor IDs does not fit this skill. Use a sparse vector or registry keyed by sensor ID.

## Example dossiers

- `examples/good-request.md` -> minimal trigger shape.
- `examples/stack-exemplars.md` -> stronger repo-native precedents elsewhere in the stack.
- `examples/source-corpus-exemplars.md` -> repo-local canonical structures.
