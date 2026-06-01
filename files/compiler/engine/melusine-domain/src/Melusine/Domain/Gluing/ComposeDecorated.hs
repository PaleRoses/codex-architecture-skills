module Melusine.Domain.Gluing.ComposeDecorated
  ( CompositionResult (..),
    StructuredComposeDecorated (..),
    composeDecorated,
    composeDecoratedStructured,
    composeStructuredDecoratedCospan,
  )
where

import Melusine.Domain.Core.Class (DomainAlgebra (..))
import qualified Moonlight.Category as Category
import Moonlight.Category (CompositionResult (..), HasPushouts, StructuredCospan (..))

class (DomainAlgebra d, HasPushouts (BoundaryCategory d)) => StructuredComposeDecorated d where
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

composeDecorated ::
  DomainAlgebra d =>
  (Decoration d -> Decoration d -> Decoration d) ->
  Boundary d ->
  (d, Decoration d) ->
  (d, Decoration d) ->
  CompositionResult d (CompositionObligation d) (Decoration d)
composeDecorated combineDecorations boundaryValue leftValue rightValue =
  Category.composeDecorated combineDecorations glue boundaryValue leftValue rightValue

composeDecoratedStructured ::
  forall d.
  StructuredComposeDecorated d =>
  (Decoration d -> Decoration d -> Decoration d) ->
  Boundary d ->
  (d, Decoration d) ->
  (d, Decoration d) ->
  Maybe (CompositionResult d (CompositionObligation d) (Decoration d))
composeDecoratedStructured combineDecorations boundaryValue leftValue rightValue =
  Category.composeDecoratedStructured
    ( Category.StructuredCompositionAlgebra
        { Category.toStructuredBoundary = toStructuredBoundary @d,
          Category.fromStructuredComposition = fromStructuredComposition @d
        }
    )
    combineDecorations
    boundaryValue
    leftValue
    rightValue

composeStructuredDecoratedCospan ::
  HasPushouts category =>
  (leftDecoration -> rightDecoration -> combinedDecoration) ->
  StructuredCospan category leftDecoration ->
  StructuredCospan category rightDecoration ->
  Maybe (StructuredCospan category combinedDecoration)
composeStructuredDecoratedCospan =
  Category.composeStructuredDecoratedCospan
