# Quality Gates

A design passes only if all gates are satisfied.

1. The repeated shape has one semantic owner.
2. The field or witness universe is closed, or the design explicitly switches to a registry.
3. Every carrier parameter has named semantics.
4. Partial carriers resolve at one constructor boundary.
5. Heterogeneous payloads are indexed by witnesses or type families, not strings.
6. Field errors, sparse updates, and diagnostics carry typed field or witness evidence.
7. Total products expose lawful `tabulate` and `index` where applicable.
8. Witness-aware map/zip/traverse/fold/replace operations preserve association with the index.
9. Existential wrappers retain enough witness evidence to recover payload operations safely.
10. Dense, encoded, diagnostic, and compiled forms are derived views.
11. Tests fail when a new field or witness is added without updating constructors, traversals, projections, labels, and codecs.
12. The design rejects ordinary HKT machinery when there is no repeated record/product shape.
