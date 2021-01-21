module Polyform.Tokenized.Dual where

import Prelude

import Data.List (List)
import Data.List (fromFoldable) as List
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Profunctor.Strong (class Strong)
import Polyform.Dual (Dual, dual, parser, serializer) as Polyform.Dual
import Polyform.Tokenized (Tokenized)
import Polyform.Tokenized (liftUntokenized, unliftUntokenized) as Tokenized

data DualD p s i o o'
  = DualD (Tokenized p i o') (o → s (List i))

derive instance functorDualD ∷ (Profunctor p) ⇒ Functor (DualD p s i o)

instance applyDualD ∷ (Applicative s, Profunctor p, Semigroupoid p, Strong p) ⇒ Apply (DualD p s i o') where
  apply (DualD fprs fser) (DualD prs ser) = DualD (fprs <*> prs) (\o → (<>) <$> fser o <*> ser o)

instance applicativeDualD ∷ (Applicative s, Category p, Profunctor p, Strong p) ⇒ Applicative (DualD p s i o') where
  pure a = DualD (pure a) (const $ pure mempty)

instance profunctorDualD ∷ (Profunctor p) ⇒ Profunctor (DualD p s i) where
  dimap l r (DualD prs ser) = DualD (map r prs) (l >>> ser)

newtype Dual p s i o = Dual (DualD p s i o o)
derive instance newtypeDual ∷ Newtype (Dual p s i o) _

dual ∷ ∀ i o p s. Tokenized p i o → (o → s (List i)) → Dual p s i o
dual p s = Dual (DualD p s)

pureDual ∷ ∀ i o p s. Category p ⇒ Strong p ⇒ Applicative s ⇒ o → Dual p s i o
pureDual o = Dual (pure o)

diverge ∷
  ∀ i o o' p s.
  Functor (p i) ⇒
  Profunctor p ⇒
  (o' → o) →
  Dual p s i o →
  DualD p s i o' o
diverge f = lcmap f <<< unwrap

infixl 5 diverge as ~

parser ∷ ∀ i o p s. Dual p s i o → Tokenized p i o
parser (Dual (DualD prs _)) = prs

serializer ∷ ∀ i o p s. Dual p s i o → (o → s (List i))
serializer (Dual (DualD _ ser)) = ser

liftUntokenized ∷ ∀ i o p s. Functor s ⇒ Strong p ⇒ Polyform.Dual.Dual p s (Maybe i) o → Dual p s i o
liftUntokenized d = dual
  (Tokenized.liftUntokenized $ Polyform.Dual.parser d)
  (map List.fromFoldable <$> Polyform.Dual.serializer d)

unliftUntokenized ∷ ∀ i o p s. Profunctor p ⇒ Dual p s i o → Polyform.Dual.Dual p s (List i) o
unliftUntokenized d = Polyform.Dual.dual
  (Tokenized.unliftUntokenized $ parser d)
  (serializer d)


