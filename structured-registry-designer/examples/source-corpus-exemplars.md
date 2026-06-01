# Portable source-shaped exemplars

Use these to classify registry code before choosing a structure.

## Closed descriptor table

```haskell
data Channel = Temperature | Humidity | Pressure
data Descriptor a = Descriptor
  { descriptorKey :: Channel
  , descriptorName :: Text
  , descriptorUnit :: Unit
  }
```

Shape: total registry over a closed discrete key universe.

Transfer rule: expose total `descriptorFor :: Channel -> Descriptor`, not partial string lookup.

## Scoped environment

```haskell
data Scope = Global | Module ModuleName | Local ModuleName BlockId
data ScopeArrow = Includes Scope Scope
```

Shape: presheaf or restriction structure when values visible at a smaller scope are inherited or restricted from a larger scope.

Transfer rule: parent fallback is not lookup. It is restriction along a named scope relation with deterministic conflict policy.

## Import graph

```haskell
data Module = Module ModuleName
data Import = Import Module Module
```

Shape: category or graph-indexed registry when declarations move through imports.

Transfer rule: name the arrow as import visibility, then test identity and composition. Do not hide import behavior in map merging.

## Distributed fragments with overlaps

```haskell
data Cover i = Cover [Patch i]
data Overlap i = Overlap i i
```

Shape: sheaf-like registry when compatible local sections must glue into one global view.

Transfer rule: require overlap agreement before gluing and report the obstruction as data when agreement fails.

## Key-dependent payloads

```haskell
data Capability a where
  ReadUser  :: Capability User
  WriteUser :: Capability UserPatch
```

Shape: fibred or indexed registry when each key has its own payload category.

Transfer rule: existential packaging is lawful only when the capability witness recovers the payload type and operations.
