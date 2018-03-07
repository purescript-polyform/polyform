module Polyform.Form.Component where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Polyform.Validation (V(..), Validation(..))
import Polyform.Validation as Validation

-- | Tiny wrapper around validation which also
-- | carries default "form" value.
newtype Component m form i o =
  Component
    { validation ∷ Validation m form i o
    , default ∷ form
    }
derive instance newtypeComponent ∷ Newtype (Component m e a b) _
derive instance functorComponent ∷ (Functor m) ⇒ Functor (Component m e a)

instance applyComponent ∷ (Semigroup e, Monad m) ⇒ Apply (Component m e a) where
  apply (Component rf) (Component ra) =
    Component
      { validation: rf.validation <*> ra.validation
      , default: rf.default <> ra.default
      }

instance applicativeComponent ∷ (Monoid e, Monad m) ⇒ Applicative (Component m e a) where
  pure a = Component { validation: pure a, default: mempty }

instance altComponent ∷ (Monoid e, Monad m) ⇒ Alt (Component m e a) where
  alt (Component v1) (Component v2) =
    Component
      { validation: v1.validation <|> v2.validation
      , default: v1.default <> v2.default
      }

instance semigroupoidComponent ∷ (Monad m, Semigroup e) ⇒ Semigroupoid (Component m e) where
  compose (Component r2) (Component r1) =
    Component { default: r1.default <> r2.default, validation: r2.validation <<< r1.validation }

instance categoryComponent ∷ (Monad m, Monoid e) ⇒ Category (Component m e) where
  id = Component { validation: id, default: mempty }

instance profunctorComponent ∷ (Monad m, Monoid e) ⇒ Profunctor (Component m e) where
  dimap l r c = hoistFn l >>> c >>> hoistFn r

runValidation ∷ ∀ form i o m. Component m form i o → (i → m (V form o))
runValidation = unwrap <<< _.validation <<< unwrap

fromValidation :: forall a b e m. Monoid e => Validation m e a b -> Component m e a b
fromValidation validation = Component { validation, default: mempty }

hoistFn
  ∷ ∀ a b e m
  . Monoid e
  ⇒ Monad m
  ⇒ (a → b)
  → Component m e a b
hoistFn = fromValidation <<< Validation.hoistFn

hoistFnV
  ∷ ∀ a b e m
  . Monoid e
  ⇒ Monad m
  ⇒ (a → V e b)
  → Component m e a b
hoistFnV = fromValidation <<< Validation.hoistFnV

hoistFnMV
  ∷ ∀ a b e m
  . Monoid e
  ⇒ Monad m
  ⇒ (a → m (V e b))
  → Component m e a b
hoistFnMV = fromValidation <<< Validation.hoistFnMV

-- | Simple helper which combines basic pieces into `Component`:
-- |  - form constructor (I could use `Applicative.pure` but it seems a bit to heavy constraint ;-))
-- |  - default field value
-- |  - validation
fromField
  ∷ ∀ attrs e form m q v
  . Monad m
  ⇒ (Record (value ∷ V e v | attrs) → form)
  → Record (value ∷ V e v | attrs)
  → Validation m e q v
  → Component m form q v
fromField = fromFieldCoerce id

-- | Longer version of previous one which
-- | allows coersion of field level value
-- | into form level value.
fromFieldCoerce
  ∷ ∀ attrs e form m q v v'
  . Monad m
  ⇒ (v → v')
  → (Record (value ∷ V e v | attrs) → form)
  → Record (value ∷ V e v | attrs)
  → Validation m e q v
  → Component m form q v'
fromFieldCoerce coerce singleton field validation = Component $
  { validation: Validation $ \query → do
      r ← Validation.runValidation validation query
      pure $ case r of
        Valid e v → Valid (singleton $ field { value = Valid e v }) (coerce v)
        Invalid e → Invalid (singleton $ field { value = Invalid e })
  , default: singleton field
  }
