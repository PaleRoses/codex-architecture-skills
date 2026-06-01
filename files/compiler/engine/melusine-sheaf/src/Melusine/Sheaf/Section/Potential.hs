module Melusine.Sheaf.Section.Potential
  ( -- * Synthesis configuration
    PotentialSynthesis (..),
    defaultPotentialSynthesis,

    -- * Convenience weight functions
    uniformClimatePotential,

    -- * Core evaluation
    evaluatePotentialAtCell,
    synthesizeScalarPotential,

    -- * Error types
    PotentialSynthesisError (..),
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Melusine.Algebra
  ( ChannelState,
    ChannelWitness (..),
    ClimateVec,
    CreaturePopulations,
    EnvironmentTags,
    InfluenceField,
    LoreFacts,
    RuleState,
    climateToEntries,
    foldMapChannelVecWithWitness,
  )
import Melusine.Sheaf.Site.Stalk
  ( Stalk,
    StalkState (..),
  )
import Moonlight.Homology
  ( CellCarrier,
    BasisCellRef,
    PotentialNormalization (..),
    ScalarPotentialField,
    ScalarPotentialFieldError,
    carrierCells,
    mkScalarPotentialFieldFromSamples,
  )

-- | A product of monoid homomorphisms from each stalk channel's algebraic
-- structure to @(Sum Double)@. Each field projects one channel's value to a
-- scalar contribution; the synthesis sums all six contributions per cell.
--
-- Categorically, this is a natural transformation from the stalk functor
-- (a product of six representable functors over the channel category) to the
-- constant functor at @Double@ — composed with the forgetful functor from
-- @Sum Double@ to @Double@. One might call it a "generalized character" of
-- the stalk algebra, but that would require the audience to have read
-- Milewski, which is evidently too much to ask.
data PotentialSynthesis = PotentialSynthesis
  { climatePotential :: ClimateVec -> Double,
    influencePotential :: InfluenceField -> Double,
    tagPotential :: EnvironmentTags -> Double,
    lorePotential :: LoreFacts -> Double,
    populationPotential :: CreaturePopulations -> Double,
    rulePotential :: RuleState -> Double
  }

-- | The zero morphism. Every channel maps to @0.0@. Useful as a starting
-- point for building a synthesis by overriding individual channel weights.
-- If you use this unmodified, you deserve the flat potential field you get.
defaultPotentialSynthesis :: PotentialSynthesis
defaultPotentialSynthesis =
  PotentialSynthesis
    { climatePotential = const 0.0,
      influencePotential = const 0.0,
      tagPotential = const 0.0,
      lorePotential = const 0.0,
      populationPotential = const 0.0,
      rulePotential = const 0.0
    }

-- | Weight all 8 climate axes equally by a single coefficient.
-- Sums the non-zero axis values from the sparse representation and scales
-- by the coefficient. For when one cannot be bothered to distinguish
-- temperature from luminosity — a questionable aesthetic choice, but a
-- valid linear functional on the free module nonetheless.
uniformClimatePotential :: Double -> ClimateVec -> Double
uniformClimatePotential coefficient climateVec =
  coefficient * getSum (foldMap (Sum . snd) (climateToEntries climateVec))

-- | Evaluate the scalar potential contribution of a single stalk.
-- This is the inner kernel of the synthesis: for each of the six channels,
-- extract the channel state from the stalk, apply the corresponding weight
-- function, and sum the results.
--
-- The fold structure is a direct application of the universal property of
-- products: given six morphisms @f_i : A_i -> M@ into a monoid @M@, there
-- exists a unique morphism from the product @A_1 x ... x A_6 -> M@.
-- The @ChannelVec@ is the product; @Sum Double@ is the monoid.
evaluatePotentialAtCell :: PotentialSynthesis -> Stalk -> Double
evaluatePotentialAtCell synthesis stalk =
  getSum
    ( foldMapChannelVecWithWitness
        ( \witness (StalkState channelValue) ->
            Sum (applyChannelWeight synthesis witness channelValue)
        )
        stalk
    )

-- | The dispatch function: given a channel witness and the channel's state,
-- select the appropriate weight function from the synthesis record and apply
-- it. This is the component selector of the product morphism.
applyChannelWeight ::
  PotentialSynthesis ->
  ChannelWitness channel ->
  ChannelState channel ->
  Double
applyChannelWeight synthesis witness channelValue =
  case witness of
    LoreWitness -> lorePotential synthesis channelValue
    ClimateWitness -> climatePotential synthesis channelValue
    InfluenceWitness -> influencePotential synthesis channelValue
    PopulationsWitness -> populationPotential synthesis channelValue
    TagsWitness -> tagPotential synthesis channelValue
    RulesWitness -> rulePotential synthesis channelValue

-- | Errors that can arise during scalar potential synthesis.
data PotentialSynthesisError
  = -- | A cell present in the carrier has no corresponding stalk in the
    -- provided stalk map. The carrier and the section are out of alignment
    -- — a configuration error, not a computational one.
    CellMissingFromStalkMap BasisCellRef
  | -- | The carrier contains no cells. Synthesizing a potential field over
    -- the empty set is technically well-defined (it is the initial object
    -- in the category of potential fields) but practically useless, so we
    -- reject it to prevent downstream confusion.
    EmptyCarrier
  | -- | The terminal step — constructing the @ScalarPotentialField@ from
    -- validated samples — failed. This typically indicates non-finite values
    -- produced by the weight functions (NaN, Infinity).
    PotentialFieldConstructionFailed ScalarPotentialFieldError
  deriving stock (Eq, Show)

-- | Synthesize a scalar potential field from stalk data.
--
-- Algorithm (a straightforward foldMap — nothing here should surprise
-- anyone who has internalized the universal property of free monoids):
--
--   1. For each cell in the 'CellCarrier', look up its 'Stalk' from the
--      provided stalk map.
--   2. Apply 'evaluatePotentialAtCell' to compute the scalar value.
--   3. Collect the per-cell scalars into a @Map BasisCellRef Double@.
--   4. Feed into 'mkScalarPotentialFieldFromSamples' with native scaling.
--
-- The stalk map uses homological 'BasisCellRef' keys. The caller is responsible
-- for projecting from their domain's cell addressing scheme (e.g.,
-- @WorldCellRef@) to 'BasisCellRef' before invoking this function.
synthesizeScalarPotential ::
  PotentialSynthesis ->
  Map BasisCellRef Stalk ->
  CellCarrier ->
  Either PotentialSynthesisError ScalarPotentialField
synthesizeScalarPotential synthesis stalkMap carrier =
  let cells = carrierCells carrier
   in case cells of
        [] -> Left EmptyCarrier
        _ ->
          case collectSamples synthesis stalkMap cells of
            Left missingCell ->
              Left (CellMissingFromStalkMap missingCell)
            Right sampleMap ->
              case mkScalarPotentialFieldFromSamples carrier NativePotentialScale sampleMap of
                Left fieldError ->
                  Left (PotentialFieldConstructionFailed fieldError)
                Right field ->
                  Right field

-- | Traverse the cell list, looking up each cell's stalk and computing its
-- scalar potential. Fails fast on the first missing cell.
--
-- This is morally a @traverse@ in @Either@, which is to say the Kleisli
-- composition of lookup and evaluation in the @Either BasisCellRef@ monad.
-- I refuse to dress it up further.
collectSamples ::
  PotentialSynthesis ->
  Map BasisCellRef Stalk ->
  [BasisCellRef] ->
  Either BasisCellRef (Map BasisCellRef Double)
collectSamples synthesis stalkMap =
  foldl' accumulateSample (Right Map.empty)
  where
    accumulateSample ::
      Either BasisCellRef (Map BasisCellRef Double) ->
      BasisCellRef ->
      Either BasisCellRef (Map BasisCellRef Double)
    accumulateSample (Left err) _ = Left err
    accumulateSample (Right accumulated) cellRef =
      case Map.lookup cellRef stalkMap of
        Nothing -> Left cellRef
        Just stalk ->
          let value = evaluatePotentialAtCell synthesis stalk
           in Right (Map.insert cellRef value accumulated)
