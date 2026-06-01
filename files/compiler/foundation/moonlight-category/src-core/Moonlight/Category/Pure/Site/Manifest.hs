module Moonlight.Category.Pure.Site.Manifest
  ( mkSiteManifest,
    validateSiteManifest,
  )
where

import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Category.Pure.Site.Core (SiteManifest (..), SiteViolation (..))
import Moonlight.Category.Pure.Site.Graph (importCycles, reachableClosure)

mkSiteManifest :: Ord obj => Set obj -> Map obj (Set obj) -> Map obj (Set obj) -> Either [SiteViolation obj] (SiteManifest obj)
mkSiteManifest objects imports covers =
  let manifest = SiteManifest objects imports covers
      errors = validateSiteManifest manifest
   in if null errors
        then Right manifest
        else Left errors

validateSiteManifest :: Ord obj => SiteManifest obj -> [SiteViolation obj]
validateSiteManifest manifest =
  let objects = siteObjects manifest
      imports = siteImports manifest
      covers = siteCovers manifest
      closureMap = reachableClosure imports
      unknownImportTargets =
        Map.keys imports
          & filter (not . (`Set.member` objects))
          & fmap UnknownImportTarget
      unknownImportedObjects =
        Map.toList imports
          >>= ( \(targetObj, sources) ->
                  Set.toList sources
                    & filter (not . (`Set.member` objects))
                    & fmap (UnknownImportedObject targetObj)
              )
      unknownCoverTargets =
        Map.keys covers
          & filter (not . (`Set.member` objects))
          & fmap UnknownCoverTarget
      unknownCoveredObjects =
        Map.toList covers
          >>= ( \(targetObj, sources) ->
                  Set.toList sources
                    & filter (not . (`Set.member` objects))
                    & fmap (UnknownCoveredObject targetObj)
              )
      missingCovers =
        Set.toList objects
          & filter (\obj -> Map.notMember obj covers)
          & fmap MissingCover
      coverOutsideReachable =
        Map.toList covers
          >>= ( \(targetObj, coverSet) ->
                  let reachable = Map.findWithDefault Set.empty targetObj closureMap
                      outside = Set.difference coverSet reachable
                   in if Set.null outside
                        then []
                        else [CoverOutsideReachable targetObj outside]
              )
      coverClosureViolations =
        Map.toList covers
          >>= ( \(targetObj, coverSet) ->
                  Set.toList coverSet
                    >>= ( \covered ->
                            let coveredCover = Map.findWithDefault Set.empty covered covers
                                missing = Set.difference coveredCover coverSet
                             in if Set.null missing
                                  then []
                                  else [CoverNotClosed targetObj covered missing]
                        )
              )
      cycleViolations =
        importCycles manifest
          & fmap ImportCycleDetected
   in unknownImportTargets
        <> unknownImportedObjects
        <> unknownCoverTargets
        <> unknownCoveredObjects
        <> missingCovers
        <> coverOutsideReachable
        <> coverClosureViolations
        <> cycleViolations
