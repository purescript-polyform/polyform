module Polyform.Validator.Dual where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Plus, empty)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor, lcmap)
import Polyform.Validator (Validator)

newtype DualD m e i o' o = DualD
  { validator ∷ Validator m e i o
  , serializer ∷ o' → i
  }

derive instance newtypeDualD ∷ Newtype (DualD m e i o' o) _
derive instance functorDualD ∷ (Functor m) ⇒ Functor (DualD m e i o')

instance applyDualD ∷ (Semigroup e, Semigroup i, Monad m) ⇒ Apply (DualD m e i o') where
  apply (DualD rf) (DualD ra) = DualD
    { validator: rf.validator <*> ra.validator
    , serializer: rf.serializer <> ra.serializer
    }

instance applicativeDualD ∷ (Monoid e, Monoid i, Monad m) ⇒ Applicative (DualD m e i o') where
  pure a = DualD { validator: pure a, serializer: const mempty }

instance altDualD ∷ (Semigroup e, Monad m) ⇒ Alt (DualD m e i o') where
  alt (DualD v1) (DualD v2) =
    DualD
      { validator: v1.validator <|> v2.validator
      , serializer: v1.serializer
      }

instance plusDualD ∷ (Monoid i, Monoid e, Monad m) ⇒ Plus (DualD m e i o') where
  empty = DualD { validator: empty, serializer: const mempty }

instance profunctorDualD ∷ (Functor m, Monoid e) ⇒ Profunctor (DualD m e i) where
  dimap l r (DualD { serializer, validator }) =
    DualD
      { serializer: l >>> serializer
      , validator: map r validator
      }

newtype Dual m form i o =
  Dual (DualD m form i o o)
derive instance newtypeDual ∷ Newtype (Dual m e a b) _

instance semigroupoidDual ∷ (Monad m, Semigroup e) ⇒ Semigroupoid (Dual m e) where
  compose (Dual (DualD r2)) (Dual (DualD r1)) =
    Dual <<< DualD $
      { serializer: r1.serializer <<< r2.serializer
      , validator: r2.validator <<< r1.validator
      }

instance categoryDual ∷ (Monad m, Monoid e) ⇒ Category (Dual m e) where
  identity = Dual $ DualD { validator: identity, serializer: identity }

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
  ∷ ∀ e i o o' m
  . Functor m
  ⇒ Monoid e
  ⇒ (o' → o)
  → Dual m e i o
  → DualD m e i o' o
diverge f = lcmap f <<< unwrap
