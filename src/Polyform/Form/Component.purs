module Polyform.Form.Component where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Polyform.Field as Field
import Polyform.Form.Validation (V(..), Validation(..))
import Polyform.Form.Validation as Form.Validation

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

instance altComponent ∷ (Semigroup e, Alt m) ⇒ Alt (Component m e a) where
  alt (Component r1) (Component r2) = Component
    { validation: r1.validation <|> r2.validation
    , default: r1.default <> r2.default
    }

instance semigroupoidComponent ∷ (Monad m, Semigroup e) ⇒ Semigroupoid (Component m e) where
  compose (Component r2) (Component r1) =
    Component { default: r1.default <> r2.default, validation: r2.validation <<< r1.validation }

instance categoryComponent ∷ (Monad m, Monoid e) ⇒ Category (Component m e) where
  id = Component { validation: id, default: mempty }

validate ∷ ∀ form i o m. Component m form i o → (i → m (V form o))
validate = unwrap <<< _.validation <<< unwrap

fromValidation :: forall a b e m. Monoid e => Validation m e a b -> Component m e a b
fromValidation validation = Component { validation, default: mempty }

liftV
  ∷ ∀ a b e m
  . Monoid e
  ⇒ Monad m
  ⇒ (a → V e b)
  → Component m e a b
liftV = fromValidation <<< Form.Validation.liftV

-- | Simple helper which combines basic pieces into `Component`:
-- |  - form constructor (I could use `Applicative.pure` but it seems a bit to heavy constraint ;-))
-- |  - default field value
-- |  - validation
fromField
  ∷ ∀ attrs e form m q v
  . Monad m
  ⇒ (Record (value ∷ Either e v | attrs) → form)
  → Record (value ∷ Either e v | attrs)
  → Field.Validation m e q v
  → Component m form q v
fromField singleton field validation = Component $
  { validation: Validation $ \query → do
      r ← runExceptT (Field.runValidation validation query)
      pure $ case r of
        Left e → Invalid (singleton $ field { value = Left e })
        Right v → Valid (singleton $ field { value = Right v }) v
  , default: singleton field
  }

