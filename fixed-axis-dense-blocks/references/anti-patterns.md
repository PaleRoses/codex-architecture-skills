# Anti-patterns

- Dense block for open or sparse axes.
- Row-major and column-major both exposed as semantic options.
- Mutable and immutable blocks share ownership outside a scoped mutation boundary.
- Axis count is passed everywhere instead of fixed by the block type or constructor.
- Unsafe indexing exposed through public API.
- Shape mismatches silently truncated.
- Public domain objects depend on the storage layout formula.
