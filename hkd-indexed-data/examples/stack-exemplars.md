# Stack Exemplars

Use these transferable shapes when the immediate source corpus is too narrow.

## HKD record

```haskell
data UserRow f = UserRow
  { urId    :: Column f UserId
  , urEmail :: Column f Email
  , urRole  :: Column f Role
  }
```

Steal when one logical record has raw, validated, encoded, or columnar forms.

Required decision:

```text
Name each carrier. Example: Maybe = partial input, Identity = canonical row, Vector = batched column, Const Text = header/label.
```

Reject when each representation has unrelated fields. HKD removes duplicated shape; it does not magically unify unrelated domains.

## Witness-indexed product

```haskell
data Family = Geometry | Material | Behavior

data FamilyWit family where
  GeometryW :: FamilyWit 'Geometry
  MaterialW :: FamilyWit 'Material
  BehaviorW :: FamilyWit 'Behavior

type family FamilyPayload family where
  FamilyPayload 'Geometry = GeometryState
  FamilyPayload 'Material = MaterialState
  FamilyPayload 'Behavior = BehaviorState

newtype At payload family = At (payload family)

data FamilyProduct payload = FamilyProduct
  { fpGeometry :: payload 'Geometry
  , fpMaterial :: payload 'Material
  , fpBehavior :: payload 'Behavior
  }
```

Steal when each member has a distinct payload type but total product operations matter.

Required operations:

```haskell
tabulateFamilyProduct
  :: (forall family. FamilyWit family -> payload family)
  -> FamilyProduct payload

indexFamilyProduct
  :: FamilyWit family
  -> FamilyProduct payload
  -> payload family

traverseFamilyProductWithWitness
  :: Applicative effect
  => (forall family. FamilyWit family -> payload family -> effect (next family))
  -> FamilyProduct payload
  -> effect (FamilyProduct next)
```

Reject when the runtime lookup is `Map Text Dynamic`. That is a junk drawer with a kind signature taped to it.

## Field witness for HKD diagnostics

```haskell
data UserField a where
  UserIdField :: UserField UserId
  UserEmailField :: UserField Email
  UserRoleField :: UserField Role

data SomeUserField where
  SomeUserField :: UserField a -> SomeUserField

data UserFieldError
  = MissingUserField SomeUserField
  | InvalidUserField SomeUserField Text
```

Steal when validation, sparse updates, labels, or codecs need to name a field while preserving type-safe field access.

Reject when diagnostics use raw strings that cannot be tied back to the record shape.

## Sparse-to-total carrier

```haskell
type PartialUser = UserRow Maybe
type CanonicalUser = UserRow Identity

resolveUser :: PartialUser -> Either (NonEmpty UserFieldError) CanonicalUser
```

Steal when authoring is incomplete but runtime state must be total.

Required decision:

```text
Every missing value is either a named error or a declared default. No consumer-side "if absent then..." seepage.
```

## Dense derived view

```haskell
toDense :: FamilyProduct Payload -> Vector EncodedCell
fromDenseChecked :: Vector EncodedCell -> Either DenseProjectionError (FamilyProduct Payload)
```

Steal only when the dense view is downstream of the witness owner.

Required tests:

```text
index (tabulate f) = f
map id = id
replace changes one member
sparse resolution reports every missing required field
dense projection round trips or declares one-way loss
adding a new field or witness breaks tests until all derived surfaces are updated
```

## Decision table

| Shape | Use | Reject |
| --- | --- | --- |
| `Record carrier` | Same fields across raw/canonical/encoded/vectorized views | one-off record with no repeated shape |
| `FieldWitness a` | typed labels, diagnostics, updates, sparse attribution | string keys for known fields |
| `Product witness payload` | closed family with heterogeneous payloads | open plugin registry |
| `CoveringProduct witness payload` | generic total product over witnesses | ad hoc product copies per subset |
| Dense/vector view | hot execution projection | canonical schema authority |
