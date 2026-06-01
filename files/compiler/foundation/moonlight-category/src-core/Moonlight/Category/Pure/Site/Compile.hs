module Moonlight.Category.Pure.Site.Compile
  ( ThinSitePresentation (..),
    ThinSiteKernel (..),
    ThinSiteLookupError (..),
    thinSitePresentation,
    thinPresentationToFinCat,
    thinSiteKernel,
    thinSiteFinObject,
    thinSiteFinMorphism,
    thinSiteFinMorphismByEndpoints,
    siteAsFinCat,
  )
where

import Data.Kind (Type)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Category.Pure.Category (Category (identity))
import Moonlight.Category.Pure.FinCat
  ( FinCat,
    FinCatValidationError,
    FinMor,
    FinMorId (..),
    FinObj,
    mkFinCat,
    mkFinMorphism,
    mkFinObject,
  )
import Moonlight.Category.Pure.Site.Core (SiteFinCatError (..), SiteManifest (..))
import Moonlight.Category.Pure.Site.Graph (reachableClosure)
import Moonlight.Category.Pure.Site.Manifest (validateSiteManifest)

type ThinSitePresentation :: Type -> Type
data ThinSitePresentation obj = ThinSitePresentation
  { thinPresentationObjectIds :: Map obj Int,
    thinPresentationPairIds :: Map (obj, obj) FinMorId,
    thinPresentationObjects :: Set Int,
    thinPresentationMorphisms :: Map (Int, Int) [FinMorId],
    thinPresentationComposition :: Map (FinMorId, FinMorId) FinMorId
  }

type ThinSiteKernel :: Type -> Type
data ThinSiteKernel obj = ThinSiteKernel
  { thinSiteKernelManifest :: SiteManifest obj,
    thinSiteKernelCodomain :: FinCat,
    thinSiteKernelObjectIds :: Map obj Int,
    thinSiteKernelPairIds :: Map (obj, obj) FinMorId
  }
  deriving stock (Eq, Show)

type ThinSiteLookupError :: Type -> Type
data ThinSiteLookupError obj
  = ThinSiteUnknownObject obj
  | ThinSiteCodomainObjectMissing Int
  | ThinSiteUnknownMorphismPair obj obj
  | ThinSiteCodomainMorphismMissing FinMorId
  deriving stock (Eq, Show)

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
          >>= ( \targetObj ->
                  Map.findWithDefault Set.empty targetObj closureMap
                    & Set.toAscList
                    & fmap (\sourceObj -> (targetObj, sourceObj))
              )
          & Set.fromList
          & Set.toAscList
      pairIds =
        dependencyPairs
          & zip [0 ..]
          & fmap (\(idx, pairValue) -> (pairValue, GeneratorId idx))
          & Map.fromList
      objectsSet =
        objectIds
          & Map.elems
          & Set.fromList
      morphismMap =
        pairIds
          & Map.toList
          >>= ( \((sourceObj, targetObj), morId) ->
                  case (Map.lookup sourceObj objectIds, Map.lookup targetObj objectIds) of
                    (Just sourceId, Just targetId) -> [((sourceId, targetId), [morId])]
                    _ -> []
              )
          & Map.fromListWith (<>)
      compositionMap =
        pairIds
          & Map.toList
          >>= ( \((leftSource, leftTarget), leftId) ->
                  pairIds
                    & Map.toList
                    >>= ( \((rightSource, rightTarget), rightId) ->
                            if rightTarget == leftSource
                              then
                                case Map.lookup (rightSource, leftTarget) pairIds of
                                  Just composedId -> [((leftId, rightId), composedId)]
                                  Nothing -> []
                              else []
                        )
              )
          & Map.fromList
   in ThinSitePresentation
        { thinPresentationObjectIds = objectIds,
          thinPresentationPairIds = pairIds,
          thinPresentationObjects = objectsSet,
          thinPresentationMorphisms = morphismMap,
          thinPresentationComposition = compositionMap
        }

thinPresentationToFinCat ::
  ThinSitePresentation obj ->
  Either FinCatValidationError FinCat
thinPresentationToFinCat presentation =
  mkFinCat
    (thinPresentationObjects presentation)
    (thinPresentationMorphisms presentation)
    (thinPresentationComposition presentation)

thinSiteKernel :: Ord obj => SiteManifest obj -> Either (SiteFinCatError obj) (ThinSiteKernel obj)
thinSiteKernel manifest =
  case validateSiteManifest manifest of
    [] ->
      let presentation = thinSitePresentation manifest
       in case thinPresentationToFinCat presentation of
            Left validationError ->
              Left (SiteFinCatInvalid validationError)
            Right codomain ->
              Right
                ThinSiteKernel
                  { thinSiteKernelManifest = manifest,
                    thinSiteKernelCodomain = codomain,
                    thinSiteKernelObjectIds = thinPresentationObjectIds presentation,
                    thinSiteKernelPairIds = thinPresentationPairIds presentation
                  }
    errors ->
      Left (SiteManifestInvalid errors)

thinSiteFinObject :: Ord obj => ThinSiteKernel obj -> obj -> Either (ThinSiteLookupError obj) FinObj
thinSiteFinObject kernel objectValue =
  case Map.lookup objectValue (thinSiteKernelObjectIds kernel) of
    Nothing ->
      Left (ThinSiteUnknownObject objectValue)
    Just objectId ->
      case mkFinObject (thinSiteKernelCodomain kernel) objectId of
        Nothing ->
          Left (ThinSiteCodomainObjectMissing objectId)
        Just finObject ->
          Right finObject

thinSiteFinMorphism :: Ord obj => ThinSiteKernel obj -> NonEmpty obj -> Either (ThinSiteLookupError obj) FinMor
thinSiteFinMorphism kernel nodes =
  thinSiteFinMorphismByEndpoints
    kernel
    (NonEmpty.head nodes)
    (NonEmpty.last nodes)

thinSiteFinMorphismByEndpoints ::
  Ord obj =>
  ThinSiteKernel obj ->
  obj ->
  obj ->
  Either (ThinSiteLookupError obj) FinMor
thinSiteFinMorphismByEndpoints kernel sourceValue targetValue =
  if sourceValue == targetValue
    then identity <$> thinSiteFinObject kernel sourceValue
    else
      case Map.lookup (sourceValue, targetValue) (thinSiteKernelPairIds kernel) of
        Nothing ->
          Left (ThinSiteUnknownMorphismPair sourceValue targetValue)
        Just morId ->
          case mkFinMorphism (thinSiteKernelCodomain kernel) morId of
            Nothing ->
              Left (ThinSiteCodomainMorphismMissing morId)
            Just finMorphism ->
              Right finMorphism

siteAsFinCat :: Ord obj => SiteManifest obj -> Either (SiteFinCatError obj) FinCat
siteAsFinCat =
  fmap thinSiteKernelCodomain . thinSiteKernel
