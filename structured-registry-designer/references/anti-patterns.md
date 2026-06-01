# Anti-patterns

- Global mutable singleton registry.
- Parent fallback chain with no restriction law.
- String-keyed plugin table pretending to be a typed registry.
- Duplicate registration silently wins by insertion order.
- Child overrides that ignore intermediate scopes.
- Distributed local state glued without overlap checks.
- Runtime casts with no witness-indexed recovery path.
- Adding category-theory vocabulary without using the laws.
