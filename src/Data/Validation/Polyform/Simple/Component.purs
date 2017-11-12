module Data.Validation.Polyform.Simple.Component where

import Prelude

import Data.Functor.Invariant (class Invariant)
import Data.Validation.Polyform.Prim (V, Validation)
import Data.Validation.Polyform.Simple.Form (Form, Query)

-- | Unfortunatlly I've not found better interface for "form component" and
-- | if you want to combine results from multiple such components
-- | it requires a little work on your side.
-- | In general you should be able to combine `init` functions
-- | by just using monoidal append on them.
-- | Validation can be combined into product by using applicative instance.

-- | This component can be wrapped quite easily to provide category
-- | instance which allows you to combine multiple components
-- | and build arbitrary product value, but such an API is a bit clumsy...
-- | you can find similar type in `purescript-jaws` in `Data.Validation.Jaws.Product`.

newtype FormComponent field m a = FormComponent
  { init ∷ a → m (Form field)
  , validation ∷ Validation m (Form field) Query a
  }

instance invariantFunctorFormComponent ∷ (Functor m) ⇒ Invariant (FormComponent field m) where
  imap f g (FormComponent { init, validation }) = FormComponent { init: init <<< g, validation: f <$> validation }

-- | I'm not sure if this instance is useful as you usually want to aggregate data into custom datatype and not an
-- | monoidal structure ;-)
instance semigroupFormComponent ∷ (Semigroup a, Semigroup (m (Form field)), Semigroup (m (V (Form field) a))) ⇒ Semigroup (FormComponent field m a) where
  append (FormComponent fc1) (FormComponent fc2) =
    FormComponent { init: (fc1.init <> fc2.init), validation: fc1.validation <> fc2.validation }
