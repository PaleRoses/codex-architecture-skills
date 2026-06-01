# HKD Indexed Data Code Examples

These snippets are the code corpus for `hkd-indexed-data`. They are local to the skill so the skill folder can travel without a repo-level source tree.

## ChannelVec: closed heterogeneous product by witness

```haskell
data CanonicalChannel
  = LoreChannel
  | ClimateChannel
  | InfluenceChannel
  | PopulationsChannel
  | TagsChannel
  | RulesChannel

data ChannelWitness (channel :: CanonicalChannel) where
  LoreWitness :: ChannelWitness 'LoreChannel
  ClimateWitness :: ChannelWitness 'ClimateChannel
  InfluenceWitness :: ChannelWitness 'InfluenceChannel
  PopulationsWitness :: ChannelWitness 'PopulationsChannel
  TagsWitness :: ChannelWitness 'TagsChannel
  RulesWitness :: ChannelWitness 'RulesChannel

type family ChannelState (channel :: CanonicalChannel) where
  ChannelState 'LoreChannel = LoreFacts
  ChannelState 'ClimateChannel = ClimateVec
  ChannelState 'InfluenceChannel = InfluenceField
  ChannelState 'PopulationsChannel = CreaturePopulations
  ChannelState 'TagsChannel = EnvironmentTags
  ChannelState 'RulesChannel = RuleState

type family ChannelDeltaPayload (channel :: CanonicalChannel) where
  ChannelDeltaPayload 'LoreChannel = LoreFactsDelta
  ChannelDeltaPayload 'ClimateChannel = ClimateDelta
  ChannelDeltaPayload 'InfluenceChannel = InfluenceDelta
  ChannelDeltaPayload 'PopulationsChannel = CreaturePopulationsDelta
  ChannelDeltaPayload 'TagsChannel = TagPatch
  ChannelDeltaPayload 'RulesChannel = ConstraintDelta
```

```haskell
data SomeChannelDelta where
  SomeChannelDelta :: TypedChannelDelta channel -> SomeChannelDelta

data SomeChannelWitness where
  SomeChannelWitness :: ChannelWitness channel -> SomeChannelWitness

data ChannelConstraintBundle (constraint :: CanonicalChannel -> Constraint) =
  ChannelConstraintBundle
    { loreConstraint :: Dict (constraint 'LoreChannel)
    , climateConstraint :: Dict (constraint 'ClimateChannel)
    , influenceConstraint :: Dict (constraint 'InfluenceChannel)
    , populationsConstraint :: Dict (constraint 'PopulationsChannel)
    , tagsConstraint :: Dict (constraint 'TagsChannel)
    , rulesConstraint :: Dict (constraint 'RulesChannel)
    }
```

```haskell
data ChannelVec (f :: CanonicalChannel -> Type) = ChannelVec
  { channelLore :: f 'LoreChannel
  , channelClimate :: f 'ClimateChannel
  , channelInfluence :: f 'InfluenceChannel
  , channelPopulations :: f 'PopulationsChannel
  , channelTags :: f 'TagsChannel
  , channelRules :: f 'RulesChannel
  }

tabulateChannelVec ::
  (forall channel. ChannelWitness channel -> f channel) ->
  ChannelVec f
tabulateChannelVec buildValue =
  ChannelVec
    { channelLore = buildValue LoreWitness
    , channelClimate = buildValue ClimateWitness
    , channelInfluence = buildValue InfluenceWitness
    , channelPopulations = buildValue PopulationsWitness
    , channelTags = buildValue TagsWitness
    , channelRules = buildValue RulesWitness
    }

indexChannelVec :: ChannelWitness channel -> ChannelVec f -> f channel
indexChannelVec witness channelVecValue =
  case witness of
    LoreWitness -> channelLore channelVecValue
    ClimateWitness -> channelClimate channelVecValue
    InfluenceWitness -> channelInfluence channelVecValue
    PopulationsWitness -> channelPopulations channelVecValue
    TagsWitness -> channelTags channelVecValue
    RulesWitness -> channelRules channelVecValue
```

```haskell
traverseChannelVecWithWitness ::
  Applicative f =>
  (forall channel. ChannelWitness channel -> g channel -> f (h channel)) ->
  ChannelVec g ->
  f (ChannelVec h)
traverseChannelVecWithWitness traverseValue channelVecValue =
  ChannelVec
    <$> traverseValue LoreWitness (channelLore channelVecValue)
    <*> traverseValue ClimateWitness (channelClimate channelVecValue)
    <*> traverseValue InfluenceWitness (channelInfluence channelVecValue)
    <*> traverseValue PopulationsWitness (channelPopulations channelVecValue)
    <*> traverseValue TagsWitness (channelTags channelVecValue)
    <*> traverseValue RulesWitness (channelRules channelVecValue)

replaceChannelVec ::
  ChannelWitness channel -> f channel -> ChannelVec f -> ChannelVec f
replaceChannelVec witness replacement channelVecValue =
  case witness of
    LoreWitness -> channelVecValue {channelLore = replacement}
    ClimateWitness -> channelVecValue {channelClimate = replacement}
    InfluenceWitness -> channelVecValue {channelInfluence = replacement}
    PopulationsWitness -> channelVecValue {channelPopulations = replacement}
    TagsWitness -> channelVecValue {channelTags = replacement}
    RulesWitness -> channelVecValue {channelRules = replacement}
```

## CoveringProduct: abstract dependent product over witnesses

```haskell
type CoveringProduct :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype CoveringProduct (w :: k -> Type) (f :: k -> Type) = CoveringProduct
  { indexCoveringProduct :: forall member. w member -> f member
  }

tabulateCoveringProduct ::
  (forall member. w member -> f member) ->
  CoveringProduct w f
tabulateCoveringProduct = CoveringProduct

restrictCoveringProduct ::
  (forall member. subset member -> superset member) ->
  CoveringProduct superset f ->
  CoveringProduct subset f
restrictCoveringProduct embedWitness coveringProduct =
  tabulateCoveringProduct
    (\witness -> indexCoveringProduct coveringProduct (embedWitness witness))
```

```haskell
replaceCoveringProduct ::
  (forall left right. w left -> w right -> Maybe (left :~: right)) ->
  w member ->
  f member ->
  CoveringProduct w f ->
  CoveringProduct w f
replaceCoveringProduct sameWitness targetWitness replacement =
  adjustCoveringProduct sameWitness targetWitness (const replacement)

foldMapCoveringProductWithWitness ::
  forall k (w :: k -> Type) (f :: k -> Type) monoidValue.
  (CoveringFamily w, Monoid monoidValue) =>
  (forall member. w member -> f member -> monoidValue) ->
  CoveringProduct w f ->
  monoidValue
foldMapCoveringProductWithWitness foldValue coveringProduct =
  foldMap
    (\(Exists witness) -> foldValue witness (indexCoveringProduct coveringProduct witness))
    (allMembers @k @w)
```

## Column f a: HKD record view selection

```haskell
type family Column (f :: Type -> Type) (a :: Type) :: Type where
  Column Identity a = a
  Column VU.Vector a = VU.Vector a
  Column (Const x) _a = x

data Member f = Member
  { mTenantId :: !(Column f TenantId)
  , mUserId :: !(Column f UserId)
  , mGroupId :: !(Column f GroupId)
  }
```

```haskell
type SAtomFamily :: ((Type -> Type) -> Type) -> Type
data SAtomFamily fam = SAtomFamily
  { safAtomId :: !AtomId
  , safSchema :: ![SlotId]
  , safDecodeRow :: !(AtomRow -> Either AtomFamilyDecodeError (fam Identity))
  }

data AtomFamilyDecodeError
  = AtomFamilyDecodeRowWidthMismatch !AtomId !Int !Int
  deriving stock (Eq, Ord, Show, Read)
```

```haskell
memberFamily :: SAtomFamily Member
memberFamily =
  SAtomFamily
    { safAtomId = Rel.atomId 0
    , safSchema = [Rel.slotId 0, Rel.slotId 1, Rel.slotId 2]
    , safDecodeRow = decodeMemberRow
    }

decodeMemberRow :: Row -> Either AtomFamilyDecodeError (Member Identity)
decodeMemberRow rowValue = do
  values <- atomRowIntsExact memberFamily rowValue
  case values of
    [tenant, user, group] ->
      Right
        Member
          { mTenantId = TenantId tenant
          , mUserId = UserId user
          , mGroupId = GroupId group
          }
    _ ->
      Left
        ( AtomFamilyDecodeRowWidthMismatch
            (atomIdOf memberFamily)
            (length (schemaOf memberFamily))
            (length values)
        )
```

## Typed modality registries: dependent keys as the adjacent boundary

```haskell
type LoweringSite :: Type -> Type -> Type
data LoweringSite anchor ref
  = MissingReferences ![ref]
  | UnsupportedAnchor !anchor
  deriving stock (Eq, Ord, Show, Read)

type LoweringGap :: Type -> Type -> Type
data LoweringGap anchor ref = LoweringGap
  { lgFlavor :: !RelationFlavor
  , lgConstraintId :: !ConstraintId
  , lgSite :: !(LoweringSite anchor ref)
  }
  deriving stock (Eq, Ord, Show, Read)
```

```haskell
type ModalityRegistry :: (Type -> Type) -> Type -> Type -> Type -> Type
type ModalityRegistry key anchor result ref =
  DMap key (ObstructionModality anchor result ref)

registerModality ::
  GCompare key =>
  key value ->
  ObstructionModality anchor result ref value ->
  ModalityRegistry key anchor result ref ->
  ModalityRegistry key anchor result ref
registerModality =
  DMap.insert

evaluateModalityRegistry ::
  GCompare key =>
  ConstraintId ->
  IndexedEnvironment key ->
  ModalityRegistry key anchor result ref ->
  ModalityContribution anchor ref
```

## Cohomological substrate aliases: phase-specific views stay derived

```haskell
type SubstrateRegion substrate =
  CandidateRegion (SubstrateRoot substrate)

type SubstrateExactMatch substrate =
  CohomologicalExactMatch
    (SubstrateRoot substrate)
    (SubstrateResult substrate)
    CohomologicalCoordinate

type SubstrateWitness substrate =
  ObstructionWitness
    (SubstrateRoot substrate)
    (SubstrateRegion substrate)
    (SubstratePurpose substrate)
    (ModalityCoverage
      (SubstrateModalityTag substrate)
      RelationProjectionConflict)

type SubstrateRegionSummary substrate =
  RegionTraversalSummary
    (SubstrateRegion substrate)
    (SubstrateExactMatch substrate)
    (SubstrateGap substrate)
    (SubstrateWitness substrate)
```

```haskell
class CohomologicalSubstrate substrate where
  type SubstrateRequest substrate :: Type -> Type
  type SubstrateQuery substrate :: Type
  type SubstratePattern substrate :: Type
  type SubstrateOccurrence substrate :: Type
  type SubstrateGuard substrate :: Type
  type SubstrateCandidate substrate :: Type
  type SubstrateCapability substrate :: Type
  type SubstrateRoot substrate :: Type
  type SubstrateResult substrate :: Type
  type SubstratePurpose substrate :: Type
  type SubstrateReference substrate :: Type
  type SubstrateKernelFailure substrate :: Type
  type SubstrateSupportEvidence substrate :: Type
  type SubstrateModalityTag substrate :: Type
  type SubstrateModalityKey substrate :: Type -> Type -> Type

  type SubstrateGap substrate :: Type
  type SubstrateGap substrate =
    LoweringGap CohomologicalAnchor (SubstrateReference substrate)
```
