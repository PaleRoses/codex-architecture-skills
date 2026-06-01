{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Runtime.PublicApiSpec
  ( tests,
  )
where

import Data.Functor.Const
  ( Const,
  )
import Data.Functor.Identity
  ( Identity,
  )
import Data.Kind
  ( Type,
  )
import Data.Set qualified as Set
import Data.Vector.Unboxed qualified as VU
import Moonlight.Flow
import Moonlight.Flow qualified as Rel
import Moonlight.Flow.Model.Family
  ( AtomFamilyDecodeError (..),
    SAtomFamily (..),
    atomIdOf,
    atomRowIntsExact,
    schemaOf,
  )
import Moonlight.Flow.Runtime qualified as Runtime
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    testCase,
    (@?=),
  )

shouldRight :: Show err => Either err value -> IO value
shouldRight result =
  case result of
    Right value ->
      pure value
    Left err ->
      assertFailure (show err)

tests :: TestTree
tests =
  testGroup
    "public runtime api"
    [ testCase "constructs query/runtime path using only Moonlight.Flow" publicQueryRuntimePath,
      testCase "self-join over one source atom reads path rows" publicSelfJoinPathRows,
      testCase "self-join with identical bindings preserves source rows" publicSelfJoinIdenticalBindings,
      testCase "duplicate slot inside one match is still rejected" publicDuplicateSlotInsideMatchRejected,
      testCase "stable facade exposes checked patch construction" publicCheckedPatchFlow,
      testCase "deferred seed requires explicit hydration before reads" publicDeferredSeedHydration,
      testCase "deferred seed rejects non-empty patch application before hydration" publicDeferredSeedApplyRejected,
      testCase "typed HKD family fold decodes rows through one family witness" publicTypedFamilyFold,
      publicContextOrderTests
    ]

data PublicContext
  = PublicBottom
  | PublicLeft
  | PublicRight
  | PublicTop
  deriving stock (Eq, Ord, Show, Read)

newtype TenantId = TenantId Int
  deriving stock (Eq, Ord, Show, Read)

newtype UserId = UserId Int
  deriving stock (Eq, Ord, Show, Read)

newtype GroupId = GroupId Int
  deriving stock (Eq, Ord, Show, Read)

type family Column (f :: Type -> Type) (a :: Type) :: Type where
  Column Identity a = a
  Column VU.Vector a = VU.Vector a
  Column (Const x) _a = x

data Member f = Member
  { mTenantId :: !(Column f TenantId),
    mUserId :: !(Column f UserId),
    mGroupId :: !(Column f GroupId)
  }

deriving stock instance Eq (Member Identity)

deriving stock instance Ord (Member Identity)

deriving stock instance Show (Member Identity)

memberFamily :: SAtomFamily Member
memberFamily =
  SAtomFamily
    { safAtomId = Rel.atomId 0,
      safSchema = [Rel.slotId 0, Rel.slotId 1, Rel.slotId 2],
      safDecodeRow = decodeMemberRow
    }

decodeMemberRow :: Row -> Either AtomFamilyDecodeError (Member Identity)
decodeMemberRow rowValue = do
  values <-
    atomRowIntsExact memberFamily rowValue
  case values of
    [tenant, user, group] ->
      Right
        Member
          { mTenantId = TenantId tenant,
            mUserId = UserId user,
            mGroupId = GroupId group
          }
    _ ->
      Left
        ( AtomFamilyDecodeRowWidthMismatch
            (atomIdOf memberFamily)
            (length (schemaOf memberFamily))
            (length values)
        )

publicContextOrderTests :: TestTree
publicContextOrderTests =
  testGroup
    "public context order declarations"
    [ testCase "multi-context runtime can be created from Moonlight.Flow declarations" testPublicMultiContextRuntime,
      testCase "multi-context runtime without an order fails with a typed backend error" testPublicMultiContextWithoutOrderFails,
      testCase "invalid declared order fails with typed lattice compiler error" testPublicInvalidContextOrderFails
    ]

publicQueryRuntimePath :: Assertion
publicQueryRuntimePath = do
  let edge :: RuntimeAtom String String
      edge =
        atom (atomId 0) [slotId 0, slotId 1]
      label :: RuntimeAtom String String
      label =
        atom (atomId 1) [slotId 1, slotId 2]
      prop :: PropositionKey String
      prop =
        PropositionKey "reachable"
      ctx :: String
      ctx =
        "main"

  queryValue <-
    shouldRight
      ( query
          [ match edge,
            match label
          ]
          (select [slotId 0, slotId 2])
      )

  planValue <-
    shouldRight (plan ctx prop queryValue)

  runtime0 <-
    shouldRight
      ( createRuntime
          ( spec
              (schema [(ctx, context [edge, label] [prop])])
              [planValue]
          )
      )

  patchValue <-
    shouldRight
      ( patch
          <$> sequence
            [ insert edge (rows [[1, 10], [2, 20]]),
              insert label (rows [[10, 7], [20, 8]])
            ]
      )

  runtime1 <-
    shouldRight (applyPatch patchValue runtime0)

  outputRows <-
    shouldRight (readRows planValue runtime1)
  positiveRows outputRows @?= rows [[1, 7], [2, 8]]

  foldedCount <-
    shouldRight
      ( readRowsFold
          planValue
          runtime1
          0
          (\_row (Multiplicity multiplicity) !acc -> acc + multiplicity)
      )
  foldedCount @?= 2

publicSelfJoinPathRows :: Assertion
publicSelfJoinPathRows = do
  let sourceEdge :: RuntimeAtom String String
      sourceEdge =
        atom (atomId 0) [slotId 0, slotId 1]
      pathHead :: RuntimeAtom String String
      pathHead =
        sourceEdge
      pathTail :: RuntimeAtom String String
      pathTail =
        atom (atomId 0) [slotId 1, slotId 2]
      prop :: PropositionKey String
      prop =
        PropositionKey "path"
      ctx :: String
      ctx =
        "main"

  queryValue <-
    shouldRight
      ( query
          [ match pathHead,
            match pathTail
          ]
          (select [slotId 0, slotId 2])
      )
  planValue <-
    shouldRight (plan ctx prop queryValue)
  runtime0 <-
    shouldRight
      ( createRuntime
          ( spec
              (schema [(ctx, context [sourceEdge] [prop])])
              [planValue]
          )
      )
  patchValue <-
    shouldRight (insert sourceEdge (rows [[1, 2], [2, 3]]))
  runtime1 <-
    shouldRight (applyPatch patchValue runtime0)
  outputRows <-
    shouldRight (readRows planValue runtime1)

  positiveRows outputRows @?= rows [[1, 3]]

publicSelfJoinIdenticalBindings :: Assertion
publicSelfJoinIdenticalBindings = do
  let sourceEdge :: RuntimeAtom String String
      sourceEdge =
        atom (atomId 0) [slotId 0, slotId 1]
      prop :: PropositionKey String
      prop =
        PropositionKey "identity-self-join"
      ctx :: String
      ctx =
        "main"

  queryValue <-
    shouldRight
      ( query
          [ match sourceEdge,
            match sourceEdge
          ]
          selectAll
      )
  planValue <-
    shouldRight (plan ctx prop queryValue)
  runtime0 <-
    shouldRight
      ( createRuntime
          ( spec
              (schema [(ctx, context [sourceEdge] [prop])])
              [planValue]
          )
      )
  patchValue <-
    shouldRight (insert sourceEdge (rows [[1, 2], [2, 3]]))
  runtime1 <-
    shouldRight (applyPatch patchValue runtime0)
  outputRows <-
    shouldRight (readRows planValue runtime1)

  positiveRows outputRows @?= rows [[1, 2], [2, 3]]

publicDuplicateSlotInsideMatchRejected :: Assertion
publicDuplicateSlotInsideMatchRejected =
  case query [match (atom (atomId 0) [slotId 0, slotId 0])] selectAll of
    Left (QueryDuplicateAtomSlot duplicateAtomId duplicateSlotId) ->
      (duplicateAtomId, duplicateSlotId) @?= (atomId 0, slotId 0)
    Left err ->
      assertFailure ("expected QueryDuplicateAtomSlot, got " <> show err)
    Right _query ->
      assertFailure "duplicate slot inside one match was accepted"

publicCheckedPatchFlow :: Assertion
publicCheckedPatchFlow =
  case
    insert
      ( atom
          (atomId 7)
          [slotId 0, slotId 1]
      )
      [row [1]]
    of
      Left _err ->
        pure ()
      Right _patch ->
        assertFailure "row-width mismatch was accepted by the public checked patch constructor"

publicDeferredSeedHydration :: Assertion
publicDeferredSeedHydration = do
  let member :: RuntimeAtom String String
      member =
        atom (atomId 0) [slotId 0, slotId 1]
      prop :: PropositionKey String
      prop =
        PropositionKey "member"
      ctx :: String
      ctx =
        "tenant"

  queryValue <-
    shouldRight
      ( query
          [match member]
          selectAll
      )
  planValue <-
    shouldRight (plan ctx prop queryValue)
  seedPatch <-
    shouldRight (insert member (rows [[1, 10], [2, 20]]))

  runtime0 <-
    shouldRight
      ( createRuntimeWithOptions
          ( withInitialData
              (initialData seedPatch)
              (spec (schema [(ctx, context [member] [prop])]) [planValue])
          )
          defaultRuntimeCreateOptions
            { rcoSeedMode = RuntimeSeedDeferred
            }
      )

  case readRows planValue runtime0 of
    Left (ReadRuntimeError (RuntimeReadSeedPending _queryId)) ->
      pure ()
    Left err ->
      assertFailure ("expected pending seed read error, got " <> show err)
    Right _rows ->
      assertFailure "deferred seed read succeeded before explicit hydration"

  (runtime1, progress) <-
    shouldRight (hydrateRuntimeSeedChunk (RuntimeSeedAtoms 1) runtime0)
  progress
    @?= RuntimeSeedProgress
      { rspAppliedAtoms = 1,
        rspPendingAtoms = 0,
        rspSettled = True
      }

  outputRows <-
    shouldRight (readRows planValue runtime1)
  positiveRows outputRows @?= rows [[1, 10], [2, 20]]

publicDeferredSeedApplyRejected :: Assertion
publicDeferredSeedApplyRejected = do
  let member :: RuntimeAtom String String
      member =
        atom (atomId 0) [slotId 0, slotId 1]
      prop :: PropositionKey String
      prop =
        PropositionKey "member"
      ctx :: String
      ctx =
        "tenant"

  queryValue <-
    shouldRight
      ( query
          [match member]
          selectAll
      )
  planValue <-
    shouldRight (plan ctx prop queryValue)
  seedPatch <-
    shouldRight (insert member (rows [[1, 10]]))

  runtime0 <-
    shouldRight
      ( createRuntimeWithOptions
          ( withInitialData
              (initialData seedPatch)
              (spec (schema [(ctx, context [member] [prop])]) [planValue])
          )
          defaultRuntimeCreateOptions
            { rcoSeedMode = RuntimeSeedDeferred
            }
      )

  patchValue <-
    shouldRight (insert member (rows [[2, 20]]))
  case applyPatch patchValue runtime0 of
    Left (RuntimeApplySeedPending progress) ->
      progress
        @?= RuntimeSeedProgress
          { rspAppliedAtoms = 0,
            rspPendingAtoms = 1,
            rspSettled = False
          }
    Left err ->
      assertFailure ("expected pending seed apply error, got " <> show err)
    Right _runtime1 ->
      assertFailure "deferred seed accepted non-empty patch before explicit hydration"

publicTypedFamilyFold :: Assertion
publicTypedFamilyFold = do
  let member :: RuntimeAtom String String
      member =
        Runtime.runtimeAtomFromFamily memberFamily
      prop :: PropositionKey String
      prop =
        PropositionKey "member"
      ctx :: String
      ctx =
        "tenant"

  queryValue <-
    shouldRight
      ( query
          [match member]
          selectAll
      )
  planValue <-
    shouldRight (plan ctx prop queryValue)
  runtime0 <-
    shouldRight
      ( createRuntime
          ( spec
              (schema [(ctx, context [member] [prop])])
              [planValue]
          )
      )
  patchValue <-
    shouldRight (insert member (rows [[1, 10, 100], [2, 20, 200]]))
  runtime1 <-
    shouldRight (applyPatch patchValue runtime0)

  typedCount <-
    shouldRight
      ( Runtime.visibleRowsOfFamilyFold
          memberFamily
          planValue
          runtime1
          0
          (\_row (Multiplicity multiplicity) !acc -> acc + multiplicity)
      )
  untypedCount <-
    shouldRight
      ( readRowsFold
          planValue
          runtime1
          0
          (\_row (Multiplicity multiplicity) !acc -> acc + multiplicity)
      )
  typedCount @?= untypedCount

  typedRows <-
    shouldRight
      ( Runtime.visibleRowsOfFamilyFold
          memberFamily
          planValue
          runtime1
          Set.empty
          (\typedRow _multiplicity !acc -> Set.insert typedRow acc)
      )
  typedRows
    @?= Set.fromList
      [ Member
          { mTenantId = TenantId 1,
            mUserId = UserId 10,
            mGroupId = GroupId 100
          },
        Member
          { mTenantId = TenantId 2,
            mUserId = UserId 20,
            mGroupId = GroupId 200
          }
      ]

testPublicMultiContextRuntime :: Assertion
testPublicMultiContextRuntime =
  case createRuntime (spec orderedSchema []) of
    Right _runtime ->
      pure ()
    Left err ->
      assertFailure ("expected public multi-context runtime creation, got " <> show err)

testPublicMultiContextWithoutOrderFails :: Assertion
testPublicMultiContextWithoutOrderFails =
  case createRuntime (spec unorderedSchema []) of
    Left (RuntimeCreateBackendError (RuntimeBackendContextOrderRequired 2)) ->
      pure ()
    Left err ->
      assertFailure ("expected RuntimeBackendContextOrderRequired 2, got " <> show err)
    Right _runtime ->
      assertFailure "expected public multi-context runtime creation to require an order"

testPublicInvalidContextOrderFails :: Assertion
testPublicInvalidContextOrderFails =
  case createRuntime (spec invalidOrderedSchema []) of
    Left
      ( RuntimeCreateBackendError
          ( RuntimeBackendContextLatticeInvalid
              ( Rel.ContextLatticeUnknownEdgeEndpoint
                  (PublicBottom, PublicLeft)
                )
            )
        ) ->
        pure ()
    Left err ->
      assertFailure ("expected typed lattice compiler error, got " <> show err)
    Right _runtime ->
      assertFailure "expected invalid declared order to fail runtime creation"

orderedSchema :: RuntimeSchema PublicContext String
orderedSchema =
  schemaWithContextOrder
    publicDiamondDecl
    [ (PublicBottom, publicContextSchema),
      (PublicLeft, publicContextSchema),
      (PublicRight, publicContextSchema),
      (PublicTop, publicContextSchema)
    ]

unorderedSchema :: RuntimeSchema PublicContext String
unorderedSchema =
  schema
    [ (PublicBottom, publicContextSchema),
      (PublicTop, publicContextSchema)
    ]

invalidOrderedSchema :: RuntimeSchema PublicContext String
invalidOrderedSchema =
  schemaWithContextOrder
    (contextOrderDecl PublicTop PublicBottom [(PublicBottom, PublicLeft)])
    [ (PublicBottom, publicContextSchema),
      (PublicTop, publicContextSchema)
    ]

publicDiamondDecl :: ContextOrderDecl PublicContext
publicDiamondDecl =
  contextOrderDecl
    PublicTop
    PublicBottom
    [ (PublicBottom, PublicLeft),
      (PublicBottom, PublicRight),
      (PublicLeft, PublicTop),
      (PublicRight, PublicTop)
    ]

publicContextSchema :: RuntimeContextSchema String
publicContextSchema =
  context
    [ atom (Rel.atomId 0) [Rel.slotId 0]
    ]
    [ Rel.PropositionKey "p"
    ]
