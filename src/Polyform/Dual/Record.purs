module Polyform.Dual.Record where

import Prelude

import Polyform.Dual (Dual(..), DualD(..), dual)
import Prim.Row (class Cons, class Lacks) as Row
import Record (get) as Record
import Record.Builder (Builder) as Record
import Record.Builder (build, insert) as Record.Builder
import Type.Prelude (class IsSymbol, SProxy)

newtype Builder p s i ser prs prs' = Builder (DualD p s i ser (Record.Builder prs prs'))

instance semigroupoidProductBuilder ∷ (Semigroup (s i), Applicative (p i)) ⇒ Semigroupoid (Builder p s i ser) where
  compose (Builder (DualD prs2 ser2)) (Builder (DualD prs1 ser1)) = Builder $ DualD
    ((<<<) <$> prs2 <*> prs1)
    ((<>) <$> ser1 <*> ser2)

instance categoryProductBuilder ∷ (Monoid (s i), Applicative (p i)) ⇒ Category (Builder p s i ser) where
  identity = Builder $ DualD
    (pure identity)
    (const mempty)

insert ∷ ∀ i n o p prs prs' s ser ser'
  . Row.Cons n o ser ser'
  ⇒ Row.Lacks n ser
  ⇒ Row.Cons n o prs prs'
  ⇒ Row.Lacks n prs
  ⇒ IsSymbol n
  ⇒ Functor (p i)
  ⇒ SProxy n
  → Dual p s i o
  → Builder p s i { | ser'} ({ | prs}) ({ | prs'})
insert l (Dual (DualD prs ser)) = Builder $ DualD
  (Record.Builder.insert l <$> prs)
  (ser <<< Record.get l)

build ∷ ∀ i o p s
  . Functor (p i)
  ⇒ Builder p s i { |o} {} { |o}
  → Dual p s i { | o}
build (Builder (DualD prs ser)) = dual
  (flip Record.Builder.build {} <$> prs)
  ser


