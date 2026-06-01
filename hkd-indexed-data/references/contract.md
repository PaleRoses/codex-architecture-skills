# Contract: HKD Records and Indexed Products

## HKD record shape

Use this when the same logical fields recur across views.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

type family Column (f :: Type -> Type) (a :: Type) :: Type where
  Column Identity a = a
  Column Maybe a = Maybe a
  Column (Const x) _ = x

-- One schema, many carriers.
data Member f = Member
  { memberTenant :: !(Column f TenantId)
  , memberUser   :: !(Column f UserId)
  , memberGroup  :: !(Column f GroupId)
  }

type RawMember = Member Maybe
type CanonicalMember = Member Identity
type MemberHeader = Member (Const Text)
```

Required surfaces:

- one canonical record schema;
- named carrier meanings;
- decoding/validation from partial carriers to `Identity`;
- projection from canonical values to encoded, diagnostic, or dense views;
- no parallel hand-maintained records with the same logical fields.

## Field witness shape

Use this when errors, labels, updates, codecs, or sparse authoring need field attribution without throwing away the field type.

```haskell
data MemberField a where
  TenantField :: MemberField TenantId
  UserField :: MemberField UserId
  GroupField :: MemberField GroupId

data SomeMemberField where
  SomeMemberField :: MemberField a -> SomeMemberField

fieldLabel :: MemberField a -> Text
fieldLabel = \case
  TenantField -> "tenant"
  UserField -> "user"
  GroupField -> "group"

indexMember :: MemberField a -> Member Identity -> a
replaceMember :: MemberField a -> a -> Member Identity -> Member Identity
```

Required surfaces:

- a typed field witness when partial values, updates, or diagnostics need to name a field;
- an existential wrapper only when the witness travels with it;
- no `Text` field key as the recovery path for a known field universe.

## Witness-indexed product shape

Use this when a closed index determines heterogeneous payloads.

```haskell
data Channel = Lore | Climate | Influence

data ChannelWitness (ch :: Channel) where
  LoreW :: ChannelWitness 'Lore
  ClimateW :: ChannelWitness 'Climate
  InfluenceW :: ChannelWitness 'Influence

type family ChannelState (ch :: Channel) where
  ChannelState 'Lore = LoreFacts
  ChannelState 'Climate = ClimateVec
  ChannelState 'Influence = InfluenceField

data ChannelProduct (f :: Channel -> Type) = ChannelProduct
  { productLore :: !(f 'Lore)
  , productClimate :: !(f 'Climate)
  , productInfluence :: !(f 'Influence)
  }

tabulateChannelProduct :: (forall ch. ChannelWitness ch -> f ch) -> ChannelProduct f
indexChannelProduct :: ChannelWitness ch -> ChannelProduct f -> f ch
mapChannelProductWithWitness
  :: (forall ch. ChannelWitness ch -> f ch -> g ch)
  -> ChannelProduct f
  -> ChannelProduct g
```

Required surfaces:

- a closed witness family;
- one payload family per index;
- total `tabulate` and `index`;
- witness-aware map, zip, traverse, fold, and replace;
- existential wrappers only when paired with witnesses.

## Generic dependent product shape

Use this when the concrete closed universe should remain abstract.

```haskell
data Exists w where
  Exists :: w i -> Exists w

class CoveringFamily w where
  allMembers :: [Exists w]

newtype IndexedProduct (w :: k -> Type) (p :: k -> Type) = IndexedProduct
  { indexProduct :: forall i. w i -> p i }

tabulateProduct :: (forall i. w i -> p i) -> IndexedProduct w p
mapProduct :: (forall i. p i -> q i) -> IndexedProduct w p -> IndexedProduct w q
restrictProduct :: (forall i. sub i -> sup i) -> IndexedProduct sup p -> IndexedProduct sub p
```

Required surfaces:

- explicit membership enumeration when folding is needed;
- typed equality or witness comparison when targeted replacement is needed;
- no untyped lookup by text key.

## Sparse-to-total shape

Use this when authoring accepts incomplete fragments but the runtime product must be total.

```haskell
data FieldError field
  = MissingField field
  | DuplicateField field
  | InvalidField field Text

resolveMember
  :: Member Maybe
  -> Either (NonEmpty (FieldError SomeMemberField)) (Member Identity)
```

Required surfaces:

- missing/duplicate/invalid attribution uses field evidence;
- defaults are declared as policy, not hidden in consumers;
- the resolved product is the only canonical runtime value.

## Constructor shape

```haskell
data ResolveError field
  = MissingField field
  | DuplicateField field
  | InvalidPayload field PayloadError
  | UnknownWireId Text

resolvePartial
  :: PartialProduct
  -> Either (NonEmpty (ResolveError SomeField)) CanonicalProduct
```

## Required laws and tests

- `index witness (tabulate f) = f witness`.
- `map id = id` and `map f . map g = map (f . g)`.
- `traverse` preserves witness order and failure association.
- `replace w x` changes only `w`.
- sparse/partial construction reports every missing required member.
- dense projection round trips or documents why it is one-way.
- adding a new witness fails tests until constructors, traversals, projections, and metadata are updated.
- adding a new HKD field fails tests until field witnesses, codecs, labels, partial resolution, and projections are updated.
