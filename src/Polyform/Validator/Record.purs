module Polyform.Validator.Record where

import Prelude

import Polyform.Type.Row (class Cons') as Row
import Polyform.Validator (Validator)
import Record.Builder (Builder) as Record
import Record.Builder (build, insert) as Record.Builder
import Type.Proxy (Proxy)

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

