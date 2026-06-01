# Structured Registry Designer Code Examples

These snippets are the code corpus for `structured-registry-designer`. They live inside the skill folder so the registry examples are portable without a fake repository tree.

## Closed descriptor table: total registry over a finite universe

```haskell
data ChannelDescriptor (payload :: CanonicalChannel -> Type) (channel :: CanonicalChannel) =
  ChannelDescriptor
    { descriptorId :: ChannelId
    , descriptorPayload :: payload channel
    }

tabulateChannelDescriptors ::
  (forall channel. ChannelWitness channel -> payload channel) ->
  ChannelVec (ChannelDescriptor payload)
tabulateChannelDescriptors buildPayload =
  tabulateChannelVec
    (\witness ->
       ChannelDescriptor
         { descriptorId = channelIdFromWitness witness
         , descriptorPayload = buildPayload witness
         }
    )

descriptorAt ::
  ChannelWitness channel ->
  ChannelVec (ChannelDescriptor payload) ->
  ChannelDescriptor payload channel
descriptorAt = indexChannelVec
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

indexChannelVec :: ChannelWitness channel -> ChannelVec f -> f channel
```

## Law-bearing registry entries

```haskell
data ChannelDeltaRegistry (channel :: CanonicalChannel) = ChannelDeltaRegistry
  { registryIdentityDelta :: ChannelDeltaPayload channel
  , registryComposeDelta ::
      ChannelDeltaPayload channel ->
      ChannelDeltaPayload channel ->
      ChannelDeltaPayload channel
  , registryApplyDelta ::
      ChannelDeltaPayload channel ->
      ChannelState channel ->
      ChannelState channel
  }

channelDeltaRegistry :: ChannelWitness channel -> ChannelDeltaRegistry channel
channelDeltaRegistry witness =
  case witness of
    LoreWitness -> mkRegistry
    ClimateWitness -> mkRegistry
    InfluenceWitness -> mkRegistry
    PopulationsWitness -> mkRegistry
    TagsWitness -> mkRegistry
    RulesWitness -> mkRegistry
```

```haskell
class LawfulChannel (channel :: CanonicalChannel)

instance LawfulChannel 'LoreChannel
instance LawfulChannel 'ClimateChannel
instance LawfulChannel 'InfluenceChannel
instance LawfulChannel 'PopulationsChannel
instance LawfulChannel 'TagsChannel
instance LawfulChannel 'RulesChannel

type family ChannelLawSet (channel :: CanonicalChannel) :: [LawFamily] where
  ChannelLawSet 'LoreChannel = '[ 'ActionLaw, 'SemilatticeLaw]
  ChannelLawSet 'ClimateChannel = '[ 'ActionLaw, 'InvertibleActionLaw, 'GroupLaw]
  ChannelLawSet 'InfluenceChannel = '[ 'ActionLaw, 'SemiringLaw]
  ChannelLawSet 'PopulationsChannel = '[ 'ActionLaw, 'InvertibleActionLaw, 'GroupLaw]
  ChannelLawSet 'TagsChannel = '[ 'ActionLaw, 'BooleanLaw]
  ChannelLawSet 'RulesChannel = '[ 'ActionLaw, 'HeytingLaw]

type RequiresLaw channel law = (HasLaw channel law ~ 'True)
```

## Declared site manifests: local-to-global structure is data

```haskell
data DomainModule
  = ModuleCoreId
  | ModuleCoreTypes
  | ModuleCoreLaws
  | ModuleRegistryEntry
  | ModuleRegistryGraph
  | ModuleRegistryLoad
  | ModuleGluingComposeDecorated
  | ModuleArchitectureManifest
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type ModuleSpec = Architecture.ModuleSpec DomainModule

moduleSpecs :: Map DomainModule ModuleSpec
moduleSpecs = Architecture.moduleSpecsFromRows sourceRoot moduleRows

declaredImports :: Map DomainModule (Set DomainModule)
declaredImports = Map.map moduleImports moduleSpecs

declaredCovers :: Map DomainModule (Set DomainModule)
declaredCovers = Architecture.declaredCovers declaredImports
```

```haskell
mkSiteManifest ::
  Ord obj =>
  Set obj ->
  Map obj (Set obj) ->
  Map obj (Set obj) ->
  Either [SiteViolation obj] (SiteManifest obj)
mkSiteManifest objects imports covers =
  let manifest = SiteManifest objects imports covers
      errors = validateSiteManifest manifest
   in if null errors
        then Right manifest
        else Left errors
```

```haskell
validateSiteManifest :: Ord obj => SiteManifest obj -> [SiteViolation obj]
validateSiteManifest manifest =
  let objects = siteObjects manifest
      imports = siteImports manifest
      covers = siteCovers manifest
      closureMap = reachableClosure imports
      missingCovers =
        Set.toList objects
          & filter (\obj -> Map.notMember obj covers)
          & fmap MissingCover
      coverOutsideReachable =
        Map.toList covers
          >>= (\(targetObj, coverSet) ->
                 let reachable = Map.findWithDefault Set.empty targetObj closureMap
                     outside = Set.difference coverSet reachable
                  in if Set.null outside
                       then []
                       else [CoverOutsideReachable targetObj outside])
   in missingCovers <> coverOutsideReachable <> fmap ImportCycleDetected (importCycles manifest)
```

## Thin site compilation: arrows become an explicit category

```haskell
type ThinSitePresentation :: Type -> Type
data ThinSitePresentation obj = ThinSitePresentation
  { thinPresentationObjectIds :: Map obj Int
  , thinPresentationPairIds :: Map (obj, obj) FinMorId
  , thinPresentationObjects :: Set Int
  , thinPresentationMorphisms :: Map (Int, Int) [FinMorId]
  , thinPresentationComposition :: Map (FinMorId, FinMorId) FinMorId
  }

type ThinSiteKernel :: Type -> Type
data ThinSiteKernel obj = ThinSiteKernel
  { thinSiteKernelManifest :: SiteManifest obj
  , thinSiteKernelCodomain :: FinCat
  , thinSiteKernelObjectIds :: Map obj Int
  , thinSiteKernelPairIds :: Map (obj, obj) FinMorId
  }
```

```haskell
thinSitePresentation :: Ord obj => SiteManifest obj -> ThinSitePresentation obj
thinSitePresentation manifest =
  let objects = siteObjects manifest & Set.toAscList
      closureMap = reachableClosure (siteImports manifest)
      objectIds =
        objects
          & zip [0 ..]
          & fmap (\(idx, obj) -> (obj, idx))
          & Map.fromList
      dependencyPairs =
        objects
          >>= (\targetObj ->
                Map.findWithDefault Set.empty targetObj closureMap
                  & Set.toAscList
                  & fmap (\sourceObj -> (targetObj, sourceObj)))
          & Set.fromList
          & Set.toAscList
   in ThinSitePresentation { thinPresentationObjectIds = objectIds, ... }
```

## Presheaf-like stalk and product morphism

```haskell
newtype StalkState (channel :: CanonicalChannel) = StalkState
  { unStalkState :: ChannelState channel }

type Stalk = ChannelVec StalkState

defaultStalk :: Stalk
defaultStalk =
  tabulateChannelVec
    (\witness -> StalkState (channelDefaultFromWitness witness))

lookupStalkChannel :: ChannelWitness channel -> Stalk -> ChannelState channel
lookupStalkChannel witness stalk =
  unStalkState (indexChannelVec witness stalk)

updateStalkChannel :: ChannelWitness channel -> ChannelState channel -> Stalk -> Stalk
updateStalkChannel witness value stalk =
  replaceChannelVec witness (StalkState value) stalk
```

```haskell
data PotentialSynthesis = PotentialSynthesis
  { climatePotential :: ClimateVec -> Double
  , influencePotential :: InfluenceField -> Double
  , tagPotential :: EnvironmentTags -> Double
  , lorePotential :: LoreFacts -> Double
  , populationPotential :: CreaturePopulations -> Double
  , rulePotential :: RuleState -> Double
  }

evaluatePotentialAtCell :: PotentialSynthesis -> Stalk -> Double
evaluatePotentialAtCell synthesis stalk =
  getSum
    ( foldMapChannelVecWithWitness
        (\witness (StalkState channelValue) ->
           Sum (applyChannelWeight synthesis witness channelValue))
        stalk
    )
```

## Transport and gluing surfaces

```haskell
class Functor f c d => GeometricFunctor f c d | f -> c d where
  type ObjectGeometry f
  type TransitionGeometry f
  type GeometryError f

  objectGeometry :: f -> Ob c -> ObjectGeometry f
  transitionGeometry :: f -> Mor c -> TransitionGeometry f

  applyTransitionGeometry ::
    f ->
    TransitionGeometry f ->
    ObjectGeometry f ->
    Either (GeometryError f) (ObjectGeometry f)

  transportGeometry ::
    f -> Mor c -> ObjectGeometry f -> Either (GeometryError f) (ObjectGeometry f)
```

```haskell
class (DomainAlgebra d, HasPushouts (BoundaryCategory d)) =>
  StructuredComposeDecorated d where
  type BoundaryCategory d

  toStructuredBoundary ::
    Boundary d ->
    (d, Decoration d) ->
    Maybe (StructuredCospan (BoundaryCategory d) (Decoration d))

  fromStructuredComposition ::
    Boundary d ->
    (d, Decoration d) ->
    (d, Decoration d) ->
    StructuredCospan (BoundaryCategory d) (Decoration d) ->
    (d, [CompositionObligation d])
```
