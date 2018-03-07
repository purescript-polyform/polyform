module Polyform.Form.Component where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Polyform.Validation (V(..), Validation(..))
import Polyform.Validation as Validation

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

-- | Wraps `Polyform.Validation.AltInvalid`
newtype AltInvalid m e a b = AltInvalid (Component m e a b)
derive instance newtypeAltInvalidVaildation ∷ Newtype (AltInvalid m e a b) _
derive instance functorAltInvalid ∷ (Functor m) ⇒ Functor (AltInvalid m e a)
derive newtype instance applyAltInvalid ∷ (Semigroup e, Monad m) ⇒ Apply (AltInvalid m e a)
instance altInvalidValidation ∷ (Monoid e, Monad m) ⇒ Alt (AltInvalid m e a) where
  alt (AltInvalid (Component v1)) (AltInvalid (Component v2)) =
    AltInvalid $ Component $
      { validation:
          unwrap (Validation.AltInvalid v1.validation <|> Validation.AltInvalid v2.validation)
      , default: v1.default <> v2.default
      }

-- | Wraps `Polyform.Validation.AltValid`
newtype AltValid m e a b = AltValid (Component m e a b)
derive instance newtypeAltValidVaildation ∷ Newtype (AltValid m e a b) _
derive instance functorAltValid ∷ (Functor m) ⇒ Functor (AltValid m e a)
derive newtype instance applyAltValid ∷ (Semigroup e, Monad m) ⇒ Apply (AltValid m e a)
instance altValidValidation ∷ (Monoid e, Monad m) ⇒ Alt (AltValid m e a) where
  alt (AltValid (Component v1)) (AltValid (Component v2)) =
    AltValid $ Component $
      { validation:
          unwrap (Validation.AltValid v1.validation <|> Validation.AltValid v2.validation)
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
