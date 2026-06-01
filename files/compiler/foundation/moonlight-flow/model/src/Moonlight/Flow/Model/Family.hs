{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Model.Family
  ( SAtomFamily (..),
    AtomFamilyDecodeError (..),
    atomIdOf,
    schemaOf,
    decodeAtomFamilyRow,
    atomRowIntsExact,
  )
where

import Data.Functor.Identity
  ( Identity,
  )
import Data.Kind
  ( Type,
  )
import Moonlight.Core.Relational
  ( AtomId,
    SlotId,
  )
import Moonlight.Flow.Model.Tuple
  ( AtomRow,
    tupleKeyToInts,
    tupleKeyWidth,
  )

type SAtomFamily :: ((Type -> Type) -> Type) -> Type
data SAtomFamily fam = SAtomFamily
  { safAtomId :: !AtomId,
    safSchema :: ![SlotId],
    safDecodeRow :: !(AtomRow -> Either AtomFamilyDecodeError (fam Identity))
  }

data AtomFamilyDecodeError
  = AtomFamilyDecodeRowWidthMismatch
      !AtomId
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show, Read)

atomIdOf :: SAtomFamily fam -> AtomId
atomIdOf =
  safAtomId
{-# INLINE atomIdOf #-}

schemaOf :: SAtomFamily fam -> [SlotId]
schemaOf =
  safSchema
{-# INLINE schemaOf #-}

decodeAtomFamilyRow ::
  SAtomFamily fam ->
  AtomRow ->
  Either AtomFamilyDecodeError (fam Identity)
decodeAtomFamilyRow =
  safDecodeRow
{-# INLINE decodeAtomFamilyRow #-}

atomRowIntsExact ::
  SAtomFamily fam ->
  AtomRow ->
  Either AtomFamilyDecodeError [Int]
atomRowIntsExact atomFamily rowValue =
  let expected =
        length (schemaOf atomFamily)
      actual =
        tupleKeyWidth rowValue
   in if actual == expected
        then Right (tupleKeyToInts rowValue)
        else
          Left
            ( AtomFamilyDecodeRowWidthMismatch
                (atomIdOf atomFamily)
                expected
                actual
            )
{-# INLINE atomRowIntsExact #-}
