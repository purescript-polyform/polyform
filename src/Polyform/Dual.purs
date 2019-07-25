module Polyform.Dual where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Plus, empty)
import Data.Functor.Invariant (class Invariant)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor, dimap, lcmap)

-- | __D__ from diverging as `o'` can be different from `o`.
-- | They join in `Dual` type which wraps `DualD` a few
-- | lines below.
data DualD p i o' o = DualD (p i o) (o' → i)

instance functorDualD ∷ (Functor (p i)) ⇒ Functor (DualD p i o) where
  map f (DualD prs ser) = DualD (map f prs) ser

instance applyDualD ∷ (Apply (p i), Semigroup i) ⇒ Apply (DualD p i o') where
  apply (DualD fprs fser) (DualD prs ser) = DualD (fprs <*> prs) (fser <> ser)

instance applicativeDualD ∷ (Applicative (p i), Monoid i) ⇒ Applicative (DualD p i o') where
  pure a = DualD (pure a) (const mempty)

instance altDualD ∷ (Alt (p i)) ⇒ Alt (DualD p i o') where
  alt (DualD prs1 ser1) (DualD prs2 _) =
    DualD (prs1 <|> prs2) ser1

instance plusDualD ∷ (Plus (p i), Alt (p i), Monoid i) ⇒ Plus (DualD p i o') where
  empty = DualD empty (const mempty)

instance profunctorDualD ∷ (Functor (p i)) ⇒ Profunctor (DualD p i) where
  dimap l r (DualD prs ser) = DualD (map r prs) (l >>> ser)

newtype Dual p i o =
  Dual (DualD p i o o)
derive instance newtypeDual ∷ Newtype (Dual p i o) _

instance invariantFunctor ∷ Functor (p i) ⇒ Invariant (Dual p i) where
  imap f g (Dual d) = Dual (dimap g f d)

dual ∷ ∀ i o p
  . (p i o)
  → (o → i)
  → Dual p i o
dual prs = Dual <<< DualD prs

parser ∷ ∀ i o p. Dual p i o → p i o
parser (Dual (DualD prs _)) = prs

serializer ∷ ∀ i o p. Dual p i o → (o → i)
serializer (Dual (DualD _ ser)) = ser

hoist ∷ ∀ i o p q. (p i ~> q i) → Dual p i o → Dual q i o
hoist f (Dual (DualD prs ser)) = dual prs' ser
  where
    prs' = f prs

instance semigroupoidDual ∷ (Semigroupoid p) ⇒ Semigroupoid (Dual p) where
  compose (Dual (DualD prs2 ser2)) (Dual (DualD prs1 ser1)) =
    dual (prs2 <<< prs1) (ser1 <<< ser2)

instance categoryDual ∷ (Category p) ⇒ Category (Dual p) where
  identity = dual identity identity

-- -- | This function provides a way to diverge component serialization
-- -- | from parsing so we are able to "divide for a moment `o` type" and
-- -- | join them later by using `Applicative` composition.
-- -- | Lets use the same symbol as in codecs for similar operation.
-- -- |
-- -- | Quick example:
-- -- |
-- -- | profile = Dual $
-- -- |   ( { email1: _, email2: _, age: _}
-- -- |     <$> _.email1 ~ emailDual
-- -- |     <*> _.email2 ~ emailDual
-- -- |     <*> _.age ~ ageDual)
-- -- |
-- -- | So in the above example we can turn let say `Dual p String Email` into
-- -- |
-- -- |  `DualD p String { email1: Email } Email`
-- -- |
-- -- | using `_.email1 ~ emailDual` and `apply` + `Dual` from above example
-- -- | "joins" these types again.
-- -- | Of course these two steps can be handled by some generic layer.
infixl 5 diverge as ~

diverge
  ∷ ∀ i o o' p
  . Functor (p i)
  ⇒ Profunctor p
  ⇒ (o' → o)
  → Dual p i o
  → DualD p i o' o
diverge f = lcmap f <<< unwrap
