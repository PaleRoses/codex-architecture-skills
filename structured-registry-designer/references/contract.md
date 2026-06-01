# Contract: Structured Registry Designer

## Decision ladder

Choose the weakest structure that preserves the semantics.

| Level | Structure | Use when | Canonical query |
|---|---|---|---|
| 0 | Validated map | Open IDs, no arrows, no semantic propagation | `lookupKey` |
| 1 | Total closed registry | Closed finite key universe, no nontrivial arrows | `descriptorAt witness` |
| 2 | Functorial registry | Values move forward along morphisms | `transportAlong` |
| 3 | Presheaf registry | Values restrict from broader context to narrower context | `restrictTo` / `visibleAt` |
| 4 | Sheaf registry | Local sections must agree on overlaps and glue uniquely | `glueCover` |
| 5 | Fibred registry | Each key has its own value category | `fiberAt` / `reindexAlong` |
| 6 | Enriched/indexed registry | Morphisms carry cost, resource, probability, trust, version, proof, or policy | enriched composition |

## Total closed registry

```haskell
data Key = A | B | C

data KeyWitness key where
  AWitness :: KeyWitness 'A
  BWitness :: KeyWitness 'B
  CWitness :: KeyWitness 'C

type family Value key

data Registry f = Registry
  { regA :: f 'A
  , regB :: f 'B
  , regC :: f 'C
  }

descriptorAt :: KeyWitness key -> Registry f -> f key
```

Use for codec registries, law registries, dense descriptor tables, default registries, and closed capability dictionaries.

## Functorial registry

```haskell
data FunctorRegistry k f = FunctorRegistry
  { valueAt  :: forall a. Object k a => f a
  , mapAlong :: forall a b. k a b -> f a -> f b
  }
```

Laws:

```text
mapAlong identity = identity
mapAlong (g . f) = mapAlong g . mapAlong f
```

## Presheaf registry

```haskell
data PresheafRegistry k section = PresheafRegistry
  { sectionAt :: forall a. Object k a => section a
  , restrict  :: forall a b. k a b -> section b -> section a
  }
```

Laws:

```text
restrict identity = identity
restrict (f . g) = restrict g . restrict f
```

Use this for scope resolvers, context dictionaries, typeclass environments, config inheritance, imports, and handler visibility.

## Sheaf registry

```haskell
data SheafRegistry k section mismatch = SheafRegistry
  { presheaf :: PresheafRegistry k section
  , agreeOnOverlap :: Overlap k u v o -> section u -> section v -> [mismatch]
  , glue :: Cover k a -> MatchingFamily k section a -> Either [mismatch] (section a)
  }
```

Required guarantees:

- Compatible local sections restrict consistently to overlaps.
- Compatible matching families glue.
- Glued section is unique under the equality policy.
- Restricting the glued section recovers the local sections.

## Fibred registry

```haskell
data FibredRegistry base fiber = FibredRegistry
  { fiberAt      :: forall k. Object base k => CategoryOf (fiber k)
  , reindexAlong :: forall a b. base a b -> fiber b -> fiber a
  , totalObject  :: forall k. fiber k -> TotalObject base fiber
  }
```

Use when the values at each key form categories and movement between keys carries meaningful morphisms.

## Required errors

Name these cases when applicable: unknown key, duplicate registration, missing object, unknown morphism, broken identity law, broken composition law, failed restriction, unsupported transport, overlap mismatch, ambiguous glue, missing fiber, invalid reindexing, and enriched cost/policy violation.
