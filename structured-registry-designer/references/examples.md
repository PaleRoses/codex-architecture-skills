# Examples

## Closed descriptor table

A codec registry over a finite payload universe is a total closed registry.

```text
Objects: payload witnesses
Arrows: identity only
Query: descriptorAt witness
Errors: impossible missing descriptor after construction
```

## Scope resolver

A parent scope making values visible in child scopes is a presheaf.

```text
Objects: scopes
Arrows: child -> parent inclusion or import
Movement: restrict parent section to child visible section
Query: visibleAt child
Laws: restriction over identity and composition
```

## Distributed configuration

Local configuration fragments over regions with shared boundaries need sheaf semantics when overlaps must agree.

```text
Objects: regions and overlaps
Covers: region families covering a deployment
Mismatch: conflicting value on overlap
Glue: assemble compatible local sections into global config
```

## Dependent module registry

A module registry where each module has its own category of exports and import morphisms is fibred.

```text
Base: module dependency category
Fiber: export category at each module
Reindexing: imported symbols projected into the importing context
```

## Example dossiers

- `examples/good-request.md` -> minimal trigger shape.
- `examples/public-haskell-exemplars.md` -> public transferable precedents.
- `examples/source-corpus-exemplars.md` -> repo-local canonical structures.
