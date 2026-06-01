module Melusine.Domain.Architecture.Manifest
  ( DomainModule (..),
    ModuleLayer (..),
    ModuleSpec,
    moduleSpecs,
    moduleName,
    modulePath,
    moduleLayer,
    moduleImports,
    moduleByName,
    declaredImports,
    declaredCovers,
    moduleCover,
    allowedImport,
    declaredSiteManifest,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Melusine.Algebra.Pure.Architecture.Manifest
  ( ModuleLayer (..),
    moduleImports,
    moduleLayer,
    moduleName,
    modulePath,
  )
import qualified Melusine.Algebra.Pure.Architecture.Manifest as Architecture
import Moonlight.Category (SiteManifest, SiteViolation, mkSiteManifest)

data DomainModule
  = ModuleCoreId
  | ModuleCoreTypes
  | ModuleCoreLaws
  | ModuleCoreStableHash
  | ModuleGluingSpecDelta
  | ModuleCoreClass
  | ModuleCoreStructural
  | ModuleCorePolynomial
  | ModuleCoreDynamics
  | ModuleCoreArena
  | ModuleCoreSelector
  | ModuleCoreWiringCore
  | ModuleCoreWiringAlgebra
  | ModuleCoreWiringCompile
  | ModuleCoreWiringTyped
  | ModuleRegistryEntry
  | ModuleRegistryGraph
  | ModuleRegistryLoad
  | ModuleGluingComposeDecorated
  | ModuleGluingDecorated
  | ModuleGluingRefinement
  | ModuleArtifactIdentity
  | ModuleArtifactBundle
  | ModuleArtifactCacheKey
  | ModuleArtifactLocomotion
  | ModuleArtifactMaterial
  | ModuleGluingSpecBridge
  | ModuleEffectLawNames
  | ModuleEffectHarness
  | ModuleArchitectureManifest
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type ModuleSpec = Architecture.ModuleSpec DomainModule

sourceRoot :: FilePath
sourceRoot = "src"

siteRows :: Map DomainModule (Architecture.ModuleSpecRow DomainModule)
siteRows =
  Architecture.moduleSpecRows
    [ (ModuleCoreId, Architecture.moduleSpecRow "Melusine.Domain.Core.Id" "Melusine/Domain/Core/Id.hs" SiteLayer Set.empty),
      (ModuleCoreTypes, Architecture.moduleSpecRow "Melusine.Domain.Core.Types" "Melusine/Domain/Core/Types.hs" SiteLayer Set.empty),
      (ModuleCoreLaws, Architecture.moduleSpecRow "Melusine.Domain.Core.Laws" "Melusine/Domain/Core/Laws.hs" SiteLayer Set.empty),
      (ModuleCoreStableHash, Architecture.moduleSpecRow "Melusine.Domain.Core.StableHash" "Melusine/Domain/Core/StableHash.hs" SiteLayer (Set.fromList [ModuleCoreId, ModuleCoreTypes]))
    ]

sectionRows :: Map DomainModule (Architecture.ModuleSpecRow DomainModule)
sectionRows =
  mempty

gluingRows :: Map DomainModule (Architecture.ModuleSpecRow DomainModule)
gluingRows =
  Architecture.moduleSpecRows
    [ (ModuleGluingSpecDelta, Architecture.moduleSpecRow "Melusine.Domain.Gluing.SpecDelta" "Melusine/Domain/Gluing/SpecDelta.hs" GluingLayer Set.empty),
      (ModuleCoreClass, Architecture.moduleSpecRow "Melusine.Domain.Core.Class" "Melusine/Domain/Core/Class.hs" GluingLayer (Set.singleton ModuleGluingSpecDelta)),
      (ModuleCoreStructural, Architecture.moduleSpecRow "Melusine.Domain.Core.Structural" "Melusine/Domain/Core/Structural.hs" GluingLayer (Set.fromList [ModuleCoreClass, ModuleCoreTypes])),
      (ModuleCorePolynomial, Architecture.moduleSpecRow "Melusine.Domain.Core.Polynomial" "Melusine/Domain/Core/Polynomial.hs" GluingLayer (Set.fromList [ModuleCoreStructural, ModuleCoreTypes])),
      (ModuleCoreDynamics, Architecture.moduleSpecRow "Melusine.Domain.Core.Dynamics" "Melusine/Domain/Core/Dynamics.hs" GluingLayer (Set.fromList [ModuleCoreClass, ModuleGluingRefinement, ModuleGluingSpecDelta])),
      (ModuleCoreArena, Architecture.moduleSpecRow "Melusine.Domain.Core.Arena" "Melusine/Domain/Core/Arena.hs" GluingLayer (Set.fromList [ModuleCoreClass, ModuleCoreDynamics, ModuleCoreWiringAlgebra, ModuleCoreWiringCompile, ModuleCoreWiringCore])),
      (ModuleCoreSelector, Architecture.moduleSpecRow "Melusine.Domain.Core.Selector" "Melusine/Domain/Core/Selector.hs" GluingLayer (Set.fromList [ModuleCoreStructural, ModuleCoreTypes])),
      (ModuleCoreWiringCore, Architecture.moduleSpecRow "Melusine.Domain.Core.WiringDiagram.Core" "Melusine/Domain/Core/WiringDiagram/Core.hs" GluingLayer (Set.singleton ModuleCoreTypes)),
      (ModuleCoreWiringAlgebra, Architecture.moduleSpecRow "Melusine.Domain.Core.WiringDiagram.Algebra" "Melusine/Domain/Core/WiringDiagram/Algebra.hs" GluingLayer (Set.singleton ModuleCoreWiringCore)),
      (ModuleCoreWiringCompile, Architecture.moduleSpecRow "Melusine.Domain.Core.WiringDiagram.Compile" "Melusine/Domain/Core/WiringDiagram/Compile.hs" GluingLayer (Set.fromList [ModuleCoreWiringAlgebra, ModuleCoreWiringCore])),
      (ModuleCoreWiringTyped, Architecture.moduleSpecRow "Melusine.Domain.Core.WiringDiagram.Typed" "Melusine/Domain/Core/WiringDiagram/Typed.hs" GluingLayer (Set.fromList [ModuleCoreWiringAlgebra, ModuleCoreWiringCore])),
      (ModuleRegistryEntry, Architecture.moduleSpecRow "Melusine.Domain.Registry.Entry" "Melusine/Domain/Registry/Entry.hs" GluingLayer (Set.fromList [ModuleCoreClass, ModuleCoreId, ModuleCoreLaws, ModuleGluingSpecDelta])),
      (ModuleRegistryGraph, Architecture.moduleSpecRow "Melusine.Domain.Registry.Graph" "Melusine/Domain/Registry/Graph.hs" GluingLayer (Set.fromList [ModuleCoreId, ModuleRegistryEntry])),
      (ModuleRegistryLoad, Architecture.moduleSpecRow "Melusine.Domain.Registry.Load" "Melusine/Domain/Registry/Load.hs" GluingLayer (Set.fromList [ModuleRegistryEntry, ModuleRegistryGraph])),
      (ModuleGluingComposeDecorated, Architecture.moduleSpecRow "Melusine.Domain.Gluing.ComposeDecorated" "Melusine/Domain/Gluing/ComposeDecorated.hs" GluingLayer (Set.singleton ModuleCoreClass)),
      (ModuleGluingDecorated, Architecture.moduleSpecRow "Melusine.Domain.Gluing.Decorated" "Melusine/Domain/Gluing/Decorated.hs" GluingLayer (Set.fromList [ModuleCoreClass, ModuleGluingComposeDecorated])),
      (ModuleGluingRefinement, Architecture.moduleSpecRow "Melusine.Domain.Gluing.Refinement" "Melusine/Domain/Gluing/Refinement.hs" GluingLayer (Set.singleton ModuleCoreClass)),
      (ModuleArtifactIdentity, Architecture.moduleSpecRow "Melusine.Domain.Artifact.Identity" "Melusine/Domain/Artifact/Identity.hs" GluingLayer (Set.singleton ModuleCoreClass)),
      (ModuleArtifactBundle, Architecture.moduleSpecRow "Melusine.Domain.Artifact.Bundle" "Melusine/Domain/Artifact/Bundle.hs" GluingLayer (Set.fromList [ModuleCoreClass, ModuleCoreId, ModuleRegistryEntry])),
      (ModuleArtifactCacheKey, Architecture.moduleSpecRow "Melusine.Domain.Artifact.CacheKey" "Melusine/Domain/Artifact/CacheKey.hs" GluingLayer (Set.fromList [ModuleCoreClass, ModuleCoreId])),
      (ModuleArtifactLocomotion, Architecture.moduleSpecRow "Melusine.Domain.Artifact.Locomotion" "Melusine/Domain/Artifact/Locomotion.hs" GluingLayer (Set.singleton ModuleCoreClass)),
      (ModuleArtifactMaterial, Architecture.moduleSpecRow "Melusine.Domain.Artifact.Material" "Melusine/Domain/Artifact/Material.hs" GluingLayer (Set.singleton ModuleCoreClass)),
      (ModuleGluingSpecBridge, Architecture.moduleSpecRow "Melusine.Domain.Gluing.SpecBridge" "Melusine/Domain/Gluing/SpecBridge.hs" GluingLayer (Set.fromList [ModuleArtifactBundle, ModuleCoreClass, ModuleCoreId, ModuleGluingSpecDelta, ModuleRegistryGraph])),
      (ModuleEffectLawNames, Architecture.moduleSpecRow "Melusine.Domain.Effect.LawNames" "Melusine/Domain/Effect/LawNames.hs" GluingLayer (Set.singleton ModuleCoreLaws)),
      (ModuleEffectHarness, Architecture.moduleSpecRow "Melusine.Domain.Effect.Harness" "Melusine/Domain/Effect/Harness.hs" GluingLayer (Set.fromList [ModuleCoreClass, ModuleGluingRefinement, ModuleGluingSpecDelta]))
    ]

globalRows :: Map DomainModule (Architecture.ModuleSpecRow DomainModule)
globalRows =
  Architecture.moduleSpecRows
    [ (ModuleArchitectureManifest, Architecture.moduleSpecRow "Melusine.Domain.Architecture.Manifest" "Melusine/Domain/Architecture/Manifest.hs" GlobalLayer Set.empty)
    ]

moduleRows :: Map DomainModule (Architecture.ModuleSpecRow DomainModule)
moduleRows = mconcat [siteRows, sectionRows, gluingRows, globalRows]

moduleSpecs :: Map DomainModule ModuleSpec
moduleSpecs = Architecture.moduleSpecsFromRows sourceRoot moduleRows

moduleByName :: Map String DomainModule
moduleByName = Architecture.moduleByName moduleName moduleSpecs

declaredImports :: Map DomainModule (Set DomainModule)
declaredImports = Map.map moduleImports moduleSpecs

declaredCovers :: Map DomainModule (Set DomainModule)
declaredCovers = Architecture.declaredCovers declaredImports

moduleCover :: DomainModule -> Set DomainModule
moduleCover moduleValue =
  Map.findWithDefault Set.empty moduleValue declaredCovers

allowedImport :: ModuleLayer -> ModuleLayer -> Bool
allowedImport = Architecture.allowedImport

declaredSiteManifest :: Either [SiteViolation DomainModule] (SiteManifest DomainModule)
declaredSiteManifest =
  mkSiteManifest
    (Map.keysSet moduleSpecs)
    declaredImports
    declaredCovers
