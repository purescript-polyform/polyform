module Polyform.Dual.Record where

import Prelude

import Polyform.Dual (Dual(..), DualD(..), dual)
import Prim.Row (class Cons, class Lacks) as Row
import Record (get) as Record
import Record.Builder (Builder)
import Record.Builder (build, insert) as Builder
import Type.Prelude (class IsSymbol, SProxy)

newtype RecordBuilder p i ser prs prs' = RecordBuilder (DualD p i ser (Builder prs prs'))

instance semigroupoidProductBuilder ∷ (Semigroup i, Applicative (p i)) ⇒ Semigroupoid (RecordBuilder p i ser) where
  compose (RecordBuilder (DualD prs2 ser2)) (RecordBuilder (DualD prs1 ser1)) = RecordBuilder $ DualD
    ((<<<) <$> prs2 <*> prs1)
    ((<>) <$> ser1 <*> ser2)

instance categoryProductBuilder ∷ (Monoid i, Applicative (p i)) ⇒ Category (RecordBuilder p i ser) where
  identity = RecordBuilder $ DualD
    (pure identity)
    (const mempty)

insert ∷ ∀ i n o p prs prs' ser ser'
  . Row.Cons n o ser ser'
  ⇒ Row.Lacks n ser
  ⇒ Row.Cons n o prs prs'
  ⇒ Row.Lacks n prs
  ⇒ IsSymbol n
  ⇒ Functor (p i)
  ⇒ SProxy n
  → Dual p i o
  → RecordBuilder p i { | ser'} ({ | prs}) ({ | prs'})
insert l (Dual (DualD prs ser)) = RecordBuilder $ DualD
  (Builder.insert l <$> prs)
  (ser <<< Record.get l)

build ∷ ∀ i o p
  . Functor (p i)
  ⇒ RecordBuilder p i { |o} {} { |o}
  → Dual p i { | o}
build (RecordBuilder (DualD prs ser)) = dual
  (flip Builder.build {} <$> prs)
  ser


