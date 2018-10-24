module Polyform.Dual where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Plus, empty)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor, lcmap)
import Polyform.Validator (Validator)

newtype DualD p i o' o = DualD
  { parser ∷ p i o
  , serializer ∷ o' → i
  }

derive instance newtypeDualD ∷ Newtype (DualD p i o' o) _

instance functorDualD ∷ (Functor (p i), Functor m) ⇒ Functor (DualD p i o) where
  map f (DualD r) = DualD
    { parser: map f r.parser
    , serializer: r.serializer
    }

instance applyDualD ∷ (Functor (DualD p i o'), Apply (p i), Semigroup i) ⇒ Apply (DualD p i o') where
  apply (DualD rf) (DualD ra) = DualD
    { parser: rf.parser <*> ra.parser
    , serializer: rf.serializer <> ra.serializer
    }

instance applicativeDualD ∷ (Functor (DualD p i o'), Applicative (p i), Monoid i) ⇒ Applicative (DualD p i o') where
  pure a = DualD { parser: pure a, serializer: const mempty }

instance altDualD ∷ (Functor (DualD p i o'), Alt (p i)) ⇒ Alt (DualD p i o') where
  alt (DualD v1) (DualD v2) =
    DualD
      { parser: v1.parser <|> v2.parser
      , serializer: v1.serializer
      }

instance plusDualD ∷ (Functor (DualD p i o'), Plus (p i), Alt (p i), Monoid i) ⇒ Plus (DualD p i o') where
  empty = DualD { parser: empty, serializer: const mempty }

instance profunctorDualD ∷ (Functor (p i), Profunctor p) ⇒ Profunctor (DualD p i) where
  dimap l r (DualD d) = DualD
      { serializer: lcmap l d.serializer
      , parser: map r d.parser
      }

newtype Dual p i o =
  Dual (DualD p i o o)
derive instance newtypeDual ∷ Newtype (Dual p i o) _

instance semigroupoidDual ∷ (Semigroupoid p) ⇒ Semigroupoid (Dual p) where
  compose (Dual (DualD r2)) (Dual (DualD r1)) =
    Dual <<< DualD $
      { serializer: r1.serializer <<< r2.serializer
      , parser: r2.parser <<< r1.parser
      }

instance categoryDual ∷ (Category p) ⇒ Category (Dual p) where
  identity = Dual $ DualD { parser: identity, serializer: identity }

-- | This function provides a way to diverge component serialization
-- | from validation so we are able to "divide for a moment `o` type"
-- | and join them later by using `Applicative` composition.
-- |
-- | Quick example:
-- |
-- | profile = Dual $
-- |   ( { email1: _, email2: _, age: _}
-- |     <$> _.email1 >- emailDual
-- |     <*> _.email2 >- emailDual
-- |     <*> _.age >- ageDual)
-- |   >>> checkEmails
-- |
-- | checkEmails = hoistFnV f s
-- |   where
-- |   f r = if r.email1 != r.email2
-- |    then
-- |      errorForm "Emails don't match"
-- |    else
-- |      pure { email: r.email1, age: r.age }
-- |   s r = { email1: r.email1, email2: r.email, age: r.age }
-- |

infixl 5 diverge as >-

diverge
  ∷ ∀ i o o' p
  . Functor (p i)
  ⇒ Profunctor p
  ⇒ (o' → o)
  → Dual p i o
  → DualD p i o' o
diverge f = lcmap f <<< unwrap
