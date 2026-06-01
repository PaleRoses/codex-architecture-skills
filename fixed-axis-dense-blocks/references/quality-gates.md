# Quality Gates

1. Axis count is fixed and small.
2. Scalar type is homogeneous.
3. One canonical memory layout is documented.
4. Storage length equals `rows * axisCount`.
5. Public row value is distinct from storage block.
6. Mutable block cannot escape its scope.
7. Selected-row kernels cannot modify unselected rows.
8. Numeric kernels match reference implementations.
9. Shape validation occurs before unsafe indexing.
10. Public projection hides layout details from domain semantics.
