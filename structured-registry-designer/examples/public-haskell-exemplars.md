# Public Haskell Exemplars

These are the external precedents worth stealing for `structured-registry-designer`. They matter because they make indexing semantics explicit instead of disguising them as lookup convenience.

## Selection rule

Keep an exemplar only if it teaches one of these moves:

- witness-indexed sparse lookup;
- total closed tabulation over a declared universe;
- typed environments rather than string-keyed registries;
- declarative registries with lawful interpreters;
- explicit arrows, restriction, or transport semantics.

## 1. `dependent-map` and `dependent-sum`: sparse registries can stay typed

Sources: [`dependent-map`](https://www.stackage.org/package/dependent-map) and [`dependent-sum`](https://www.stackage.org/package/dependent-sum)

Structural lesson:

- existential packaging is acceptable only when the witness travels with the payload;
- an open sparse registry can still recover exact payload type on lookup;
- this is the honest replacement for `Map Text Dynamic`.

Reusable shape:

```haskell
data Setting a where
  RetryLimit :: Setting Int
  LogFormat  :: Setting Text
  UseTLS     :: Setting Bool

type Registry = DMap Setting Identity

lookupSetting :: Setting a -> Registry -> Maybe a
```

Steal when the registry is open or sparse but keys still determine value type.

Rejects: erased payloads recovered by casts and string tags.

## 2. `constraints` `Dict`: registry entries may need evidence, not just values

Source: [`constraints`](https://www.stackage.org/package/constraints)

Structural lesson:

- lookup sometimes must recover proof that operations are legal;
- descriptors should carry codec, equality, ordering, or law evidence deliberately;
- “the instances exist somewhere” is not a registry boundary.

Reusable shape:

```haskell
data Descriptor key = Descriptor
  { descriptorEq    :: Dict (Eq (Value key))
  , descriptorShow  :: Dict (Show (Value key))
  , descriptorCodec :: Codec (Value key)
  }
```

Steal when a registry entry is only meaningful together with the evidence that lets callers use it.

Rejects: descriptor tables that return data but cannot prove the operations they advertise.

## 3. `typerep-map`: sometimes the type is the key

Source: [TypeRep map article](https://kowainik.github.io/posts/2018-07-11-typerep-map-step-by-step)

Structural lesson:

- type-indexed lookup is a real registry design, not a curiosity;
- uniqueness and overwrite semantics must be explicit;
- performance can coexist with typed recovery.

Reusable shape:

```haskell
data Services = Services TypeMap

insertService :: Typeable a => a -> Services -> Services
lookupService :: Typeable a => Services -> Maybe a
```

Steal when users naturally ask for “the registered service of type X,” not “the value at string key X.”

Rejects: pretending type-indexed uniqueness is just a conventional map discipline.

## 4. Servant: declaration registry plus interpreter family

Source: [Servant docs](https://docs.servant.dev/)

Structural lesson:

- the canonical registry can be a declaration grammar instead of a table;
- multiple interpreters should consume one owner;
- adding docs, clients, links, or tests should not duplicate endpoint ownership.

Reusable shape:

```haskell
type API =
       "users" :> Capture "id" UserId :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] NewUser :> Post '[JSON] UserId
```

Steal when the real job is to interpret a structured declaration in several ways.

Rejects: separate route maps for server, client, docs, and validation.

## 5. `effectful` and `fused-effects`: capability lookup is scoped, not global

Sources: [`effectful`](https://hackage.haskell.org/package/effectful) and [`fused-effects`](https://hackage.haskell.org/package/fused-effects)

Structural lesson:

- handler environments have scope and order;
- shadowing is a semantic rule, not an insertion accident;
- capability lookup is usually an environment problem before it is a plugin problem.

Reusable shape:

```haskell
data HandlerStack env where
  Empty  :: HandlerStack '[]
  Extend :: Handler op -> HandlerStack env -> HandlerStack (op ': env)
```

Steal when the registry has lexical extent or staged interpretation.

Rejects: global mutable handler maps with no scope discipline.

## 6. ReaderT environment: some “registries” should just admit they are environments

Source: [ReaderT design pattern](https://www.fpcomplete.com/blog/readert-design-pattern/)

Structural lesson:

- not every dependency bag deserves categorical ornament;
- when the index structure is trivial, explicit environment threading is the honest answer.

Reusable shape:

```haskell
data Env = Env
  { envLogger :: Logger
  , envClock  :: Clock
  , envConfig :: Config
  }
```

Steal when the dependencies are fixed and the real requirement is visibility and testability.

Rejects: overengineered plugin registries for ordinary application wiring.

## Final review questions

A proposed example is strong enough only if it helps answer:

```text
What is the key universe or index category?
What arrows exist, if any?
Which way do values move?
What evidence comes back with a lookup?
When is a plain environment the better answer?
```
