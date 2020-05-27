module Polyform.Dual
  ( Dual(..)
  , DualD(..)
  , diverge
  , dual
  , dual'
  , hoistParser
  , hoistSerializer
  , parser
  , pureDual
  , serializer
  )
  where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Plus, empty)
import Data.Functor.Invariant (class Invariant)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor, dimap, lcmap)

-- | __D__ from diverging as `o'` can be different from `o`.
-- | They "join" in `Dual` type which wraps `DualD` a few
-- | lines below.
data DualD p s i o o' = DualD (p i o') (o → s i)

derive instance functorDualD ∷ (Functor (p i)) ⇒ Functor (DualD p s i o)

instance applyDualD ∷ (Apply (p i), Applicative s, Semigroup i) ⇒ Apply (DualD p s i o') where
  apply (DualD fprs fser) (DualD prs ser) = DualD (fprs <*> prs) (\i → (<>) <$> fser i <*> ser i)

instance applicativeDualD ∷ (Applicative (p i), Applicative s, Monoid i) ⇒ Applicative (DualD p s i o') where
  pure a = DualD (pure a) (const $ pure mempty)

instance altDualD ∷ (Alt (p i)) ⇒ Alt (DualD p s i o') where
  alt (DualD prs1 ser1) (DualD prs2 _) =
    DualD (prs1 <|> prs2) ser1

instance plusDualD ∷ (Plus (p i), Alt (p i), Monoid (s i)) ⇒ Plus (DualD p s i o') where
  empty = DualD empty (const mempty)

instance profunctorDualD ∷ (Functor (p i)) ⇒ Profunctor (DualD p s i) where
  dimap l r (DualD prs ser) = DualD (map r prs) (l >>> ser)

-- | `Dual` turns `DualD` into `Invariant` (it differs from `Join`).
newtype Dual p s i o = Dual (DualD p s i o o)
derive instance newtypeDual ∷ Newtype (Dual p s i o) _

instance invariantFunctor ∷ Functor (p i) ⇒ Invariant (Dual p s i) where
  imap f g (Dual d) = Dual (dimap g f d)

dual ∷ ∀ i o p s
  . (p i o)
  → (o → s i)
  → Dual p s i o
dual prs = Dual <<< DualD prs

dual' ∷ ∀ i p s. Applicative s ⇒ p i i → Dual p s i i
dual' prs = dual prs pure

parser ∷ ∀ i o p s. Dual p s i o → p i o
parser (Dual (DualD prs _)) = prs

serializer ∷ ∀ i o p s. Dual p s i o → (o → s i)
serializer (Dual (DualD _ ser)) = ser

pureDual ∷ ∀ i o p s. Applicative (p i) ⇒ Applicative s ⇒ Monoid i ⇒ o → Dual p s i o
pureDual o = Dual (pure o)

hoistSerializer ∷ ∀ i o p s s'. (s ~> s') → Dual p s i o → Dual p s' i o
hoistSerializer f (Dual (DualD prs ser)) = dual prs ser'
  where
    ser' = map f ser

hoistParser ∷ ∀ i o p q s. (p i ~> q i) → Dual p s i o → Dual q s i o
hoistParser f (Dual (DualD prs ser)) = dual prs' ser
  where
    prs' = f prs

instance semigroupoidDual ∷ (Monad s, Semigroupoid p) ⇒ Semigroupoid (Dual p s) where
  compose (Dual (DualD prs2 ser2)) (Dual (DualD prs1 ser1)) =
    dual (prs2 <<< prs1) (ser1 <=< ser2)

instance categoryDual ∷ (Category p, Monad s) ⇒ Category (Dual p s) where
  identity = dual identity pure

-- | This function provides a way to diverge component serialization
-- | from parsing so we are able to "divide for a moment `o` type" and
-- | join them later by using `Applicative` composition.
-- | Lets use the same symbol as in codecs for similar operation.
-- |
-- | Quick example:
-- |
-- | profile = Dual $
-- |   ( { email1: _, email2: _, age: _}
-- |     <$> _.email1 ~ emailDual
-- |     <*> _.email2 ~ emailDual
-- |     <*> _.age ~ ageDual)
-- |
-- | So in the above example we can turn let say `Dual p String Email` into
-- |
-- |  `DualD p String { email1: Email } Email`
-- |
-- | using `_.email1 ~ emailDual` and `apply` + `Dual` from above example
-- | "joins" these types again.
-- | Of course these two steps can be handled by some generic layer.
infixl 5 diverge as ~

diverge
  ∷ ∀ i o o' p s
  . Functor (p i)
  ⇒ Profunctor p
  ⇒ (o' → o)
  → Dual p s i o
  → DualD p s i o' o
diverge f = lcmap f <<< unwrap


