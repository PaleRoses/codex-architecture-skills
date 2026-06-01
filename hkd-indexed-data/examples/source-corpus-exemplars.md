# Portable source-shaped exemplars

Use these as shape reminders when reviewing real code. They are intentionally generic; map names to the local domain before proposing types.

## Carrier-parametric record

```haskell
data Profile f = Profile
  { profileName  :: f Text
  , profileEmail :: f Email
  , profileAge   :: f Age
  }
```

Good when the same logical fields need raw, optional, validated, diagnostic, encoded, or projected views.

Transfer rule: one field universe owns all views. `Profile Maybe`, `Profile Identity`, and `Profile (Validation FieldError)` are derived sections, not competing schemas.

## Witness-indexed product

```haskell
data Field a where
  NameField  :: Field Text
  EmailField :: Field Email
  AgeField   :: Field Age

type family FieldPayload a
```

Good when each index carries a different payload type and callers need witness-preserving `index`, `tabulate`, `traverseWithWitness`, and `replace`.

Transfer rule: existential hiding is acceptable only when the witness travels with the hidden payload.

## Phase-indexed view

```haskell
data Phase = Raw | Checked | Compiled

type family Cell (phase :: Phase) a where
  Cell Raw      a = Maybe a
  Cell Checked  a = Either FieldError a
  Cell Compiled a = a
```

Good when the same schema crosses authoring, validation, and execution phases.

Transfer rule: phase transitions are constructors with typed failures, not ad hoc record conversions.

## Sparse-to-total authoring

Use a sparse fragment only at the boundary:

```haskell
data FieldPatch = forall a. FieldPatch (Field a) a
```

Then resolve into the canonical total product once.

Transfer rule: patches and partial forms are ingestion views. The total product is the authority after construction.
