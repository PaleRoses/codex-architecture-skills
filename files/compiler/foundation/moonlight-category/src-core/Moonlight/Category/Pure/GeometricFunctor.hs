{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Category.Pure.GeometricFunctor
  ( GeometricFunctor (..),
    CoherentGeometricFunctor (..),
    transportedSourceGeometry,
    transportAlongChain,
    transportedChainGeometry,
    transportPreservesIdentityAt,
    transportPreservesCompositionAt,
  )
where

import Control.Monad (foldM)
import Data.Kind (Constraint, Type)
import Prelude hiding (Functor)
import Moonlight.Category.Pure.Category (Category (..))
import Moonlight.Category.Pure.FiniteComposable (ComposableChain (..))
import Moonlight.Category.Pure.Functor (Functor)

type GeometricFunctor :: Type -> Type -> Type -> Constraint
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
    f ->
    Mor c ->
    ObjectGeometry f ->
    Either (GeometryError f) (ObjectGeometry f)
  transportGeometry geometricFunctor morphism =
    applyTransitionGeometry
      geometricFunctor
      (transitionGeometry geometricFunctor morphism)

type CoherentGeometricFunctor :: Type -> Type -> Type -> Constraint
class GeometricFunctor f c d => CoherentGeometricFunctor f c d | f -> c d where
  type CoherenceGeometry f
  coherenceGeometry :: f -> TwoMor c -> CoherenceGeometry f

transportedSourceGeometry ::
  GeometricFunctor f c d =>
  f ->
  Mor c ->
  Either (GeometryError f) (ObjectGeometry f)
transportedSourceGeometry geometricFunctor morphism =
  transportGeometry
    geometricFunctor
    morphism
    (objectGeometry geometricFunctor (source morphism))

transportAlongChain ::
  GeometricFunctor f c d =>
  f ->
  ComposableChain c ->
  ObjectGeometry f ->
  Either (GeometryError f) (ObjectGeometry f)
transportAlongChain geometricFunctor chainValue initialGeometry =
  foldM
    (\geometryValue morphism -> transportGeometry geometricFunctor morphism geometryValue)
    initialGeometry
    (chainMorphisms chainValue)

transportedChainGeometry ::
  GeometricFunctor f c d =>
  f ->
  ComposableChain c ->
  Either (GeometryError f) (ObjectGeometry f)
transportedChainGeometry geometricFunctor chainValue =
  transportAlongChain
    geometricFunctor
    chainValue
    (objectGeometry geometricFunctor (chainStartObject chainValue))

transportPreservesIdentityAt ::
  (Eq (ObjectGeometry f), GeometricFunctor f c d) =>
  f ->
  Ob c ->
  Bool
transportPreservesIdentityAt geometricFunctor objectValue =
  case
    transportGeometry
      geometricFunctor
      (identity objectValue)
      (objectGeometry geometricFunctor objectValue)
    of
      Right transportedGeometry ->
        transportedGeometry == objectGeometry geometricFunctor objectValue
      Left _ ->
        False

transportPreservesCompositionAt ::
  (Eq (ObjectGeometry f), GeometricFunctor f c d) =>
  f ->
  Mor c ->
  Mor c ->
  Maybe Bool
transportPreservesCompositionAt geometricFunctor leftMorphism rightMorphism = do
  (composedMorphism, _) <- compose leftMorphism rightMorphism
  let sourceGeometry =
        objectGeometry geometricFunctor (source rightMorphism)
      directImage =
        transportGeometry geometricFunctor composedMorphism sourceGeometry
      iteratedImage =
        transportGeometry geometricFunctor rightMorphism sourceGeometry
          >>= transportGeometry geometricFunctor leftMorphism
  pure $
    case (directImage, iteratedImage) of
      (Right leftGeometry, Right rightGeometry) ->
        leftGeometry == rightGeometry
      _ ->
        False
