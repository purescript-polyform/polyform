module Polyform.Form.Component where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor, lmap)
import Polyform.Validation (V, Validation)
import Polyform.Validation as Validation

-- | `D` from __D__iverging
-- | because we need `o'` to be
-- | able to preserve Applicative
-- | instance.
newtype ComponentD m form i o' o =
  ComponentD
    { validation ∷ Validation m form i o
    , serialize ∷ o' → i
    , default ∷ form
    }
derive instance newtypeComponentD ∷ Newtype (ComponentD m e i o' o) _
derive instance functorComponentD ∷ (Functor m) ⇒ Functor (ComponentD m e i e')

instance applyComponentD ∷ (Semigroup e, Semigroup i, Monad m) ⇒ Apply (ComponentD m e i e') where
  apply (ComponentD rf) (ComponentD ra) =
    ComponentD
      { validation: rf.validation <*> ra.validation
      , serialize: rf.serialize <> ra.serialize
      , default: rf.default <> ra.default
      }

instance applicativeComponentD ∷ (Monoid e, Monoid i, Monad m) ⇒ Applicative (ComponentD m e i o') where
  pure a = ComponentD { validation: pure a, serialize: const mempty, default: mempty }

instance altComponentD ∷ (Monoid e, Monad m) ⇒ Alt (ComponentD m e i o') where
  alt (ComponentD v1) (ComponentD v2) =
    ComponentD
      { validation: v1.validation <|> v2.validation
      , serialize: v1.serialize
      , default: v1.default <> v2.default
      }

newtype Component m form i o =
  Component (ComponentD m form i o o)
derive instance newtypeComponent ∷ Newtype (Component m e a b) _

instance semigroupoidComponent ∷ (Monad m, Semigroup e) ⇒ Semigroupoid (Component m e) where
  compose (Component (ComponentD r2)) (Component (ComponentD r1)) =
    Component <<< ComponentD $
      { default: r1.default <> r2.default
      , serialize: r1.serialize <<< r2.serialize
      , validation: r2.validation <<< r1.validation
      }

instance categoryComponent ∷ (Monad m, Monoid e) ⇒ Category (Component m e) where
  id = Component $ ComponentD { validation: id, serialize: id, default: mempty }

instance profunctorComponentD ∷ (Functor m, Monoid e) ⇒ Profunctor (ComponentD m e i) where
  dimap l r (ComponentD { default, serialize, validation }) =
    ComponentD
      { default
      , serialize: l >>> serialize
      , validation: map r validation
      }

-- | This function provides a way to diverge component serialization
-- | from validation so we are able to "divide for a moment `o` type"
-- | and join them later by using `Applicative` composition.
-- |
-- | Quick example:
-- |
-- | profile = Component $
-- |   ( { email1: _, email2: _, age: _}
-- |     <$> _.email1 >- emailComponent
-- |     <*> _.email2 >- emailComponent
-- |     <*> _.age >- ageComponent)
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
  → Component m e i o
  → ComponentD m e i o' o
diverge f = lmap f <<< unwrap


-- runValidation ∷ ∀ form i o m. Component m form i o → (i → m (V form o))
-- runValidation = unwrap <<< _.validation <<< unwrap
--
-- fromValidation :: forall a b e m. Monoid e => Validation m e a b -> Component m e a b
-- fromValidation validation = Component { validation, default: mempty }

hoistFn
  ∷ ∀ a b e m
  . Monoid e
  ⇒ Monad m
  ⇒ (a → b)
  → (b → a)
  → Component m e a b
hoistFn f serialize = Component $ ComponentD
  { default: mempty
  , serialize
  , validation: Validation.hoistFn f
  }

hoistFnV
  ∷ ∀ a b e m
  . Monoid e
  ⇒ Monad m
  ⇒ (a → V e b)
  → (b → a)
  → Component m e a b
hoistFnV f serialize = Component $ ComponentD
  { default: mempty
  , serialize
  , validation: Validation.hoistFnV f
  }

hoistFnMV
  ∷ ∀ a b e m
  . Monoid e
  ⇒ Monad m
  ⇒ (a → m (V e b))
  → (b → a)
  → Component m e a b
hoistFnMV f serialize = Component $ ComponentD
  { default: mempty
  , serialize
  , validation: Validation.hoistFnMV f
  }

-- -- | Simple helper which combines basic pieces into `Component`:
-- -- |  - form constructor (I could use `Applicative.pure` but it seems a bit to heavy constraint ;-))
-- -- |  - default field value
-- -- |  - validation
-- fromField
--   ∷ ∀ attrs e form m q v
--   . Monad m
--   ⇒ (Record (value ∷ V e v | attrs) → form)
--   → Record (value ∷ V e v | attrs)
--   → Validation m e q v
--   → Component m form q v
-- fromField = fromFieldCoerce id
-- 
-- -- | Longer version of previous one which
-- -- | allows coersion of field level value
-- -- | into form level value.
-- fromFieldCoerce
--   ∷ ∀ attrs e form m q v v'
--   . Monad m
--   ⇒ (v → v')
--   → (Record (value ∷ V e v | attrs) → form)
--   → Record (value ∷ V e v | attrs)
--   → Validation m e q v
--   → Component m form q v'
-- fromFieldCoerce coerce singleton field validation = Component $
--   { validation: Validation $ \query → do
--       r ← Validation.runValidation validation query
--       pure $ case r of
--         Valid e v → Valid (singleton $ field { value = Valid e v }) (coerce v)
--         Invalid e → Invalid (singleton $ field { value = Invalid e })
--   , default: singleton field
--   }
