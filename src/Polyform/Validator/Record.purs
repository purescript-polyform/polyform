module Polyform.Validator.Record where

import Prelude

import Data.Map (Map)
import Data.Map (lookup, singleton) as Map
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class MappingWithIndex, mappingWithIndex)
import Polyform.Type.Row (class Cons') as Row
import Polyform.Validator (Validator, lmapValidator)
import Polyform.Validator (liftFnMaybe) as Validator
import Prim.Row (class Cons, class Lacks) as Row
import Record (insert) as Record
import Record.Builder (Builder) as Record
import Record.Builder (build, insert) as Record.Builder
import Type.Equality (from) as Type.Equality
import Type.Prelude (class IsSymbol, class TypeEquals, reflectSymbol)
import Type.Proxy (Proxy)

data Sequence = Sequence

instance
  ( Row.Cons l a r r'
  , Row.Lacks l r
  , IsSymbol l
  , Apply m
  , TypeEquals (m a) (m' a)
  ) => FoldingWithIndex Sequence (Proxy l) (m { | r }) (m' a) (m { | r'}) where
    foldingWithIndex _ l mr ma = Record.insert l <$> (Type.Equality.from ma) <*> mr

sequence :: forall m r r'. Applicative m => HFoldlWithIndex Sequence (m {}) r (m r') => r -> m r'
sequence r = hfoldlWithIndex Sequence (pure {} :: m {}) r

data MapLookup err = MapLookup (String -> err)

instance
  (IsSymbol l, Monad m, Semigroup err)
  => MappingWithIndex (MapLookup err) (Proxy l) (Validator m err i o) (Validator m (Map String err) (Map String i) o) where
  mappingWithIndex (MapLookup err) l v = do
    let
      k = reflectSymbol l
      v' = v <<< Validator.liftFnMaybe (const $ err k) (Map.lookup k)
    lmapValidator (Map.singleton k) v'

mapLookup :: forall t184 t185 t187 t188. MappingWithIndex (MapLookup t188) t187 t184 t185 => (String -> t188) -> t187 -> t184 -> t185
mapLookup mkErr = mappingWithIndex (MapLookup mkErr)


-- You can also build records one step at a time

newtype Builder m e i o o' = Builder (Validator m e i (Record.Builder o o'))

instance semigroupoidBuilder ∷ (Semigroup i, Semigroup e, Monad m) ⇒ Semigroupoid (Builder m e i) where
  compose (Builder v2) (Builder v1) = Builder $ ((<<<) <$> v2 <*> v1)

instance categoryBuilder ∷ (Monoid i, Semigroup e, Monad m) ⇒ Category (Builder m e i) where
  identity = Builder $ pure identity

insert ∷ ∀ a e i o o' n m
  . Row.Cons' n a o o'
  ⇒ Semigroup e
  ⇒ Applicative m
  ⇒ Proxy n
  → Validator m e i a
  → Builder m e i ({ | o}) ({ | o'})
insert l v = Builder $ (Record.Builder.insert l <$> v)

build ∷ ∀ e i m o
  . Applicative m
  ⇒ Semigroup e
  ⇒ Builder m e i {} { |o}
  → Validator m e i { | o}
build (Builder v) = (flip Record.Builder.build {} <$> v)

