module Data.Validation.Polyform.Simple.Component where

import Prelude

import Data.Functor.Invariant (class Invariant)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.StrMap (empty)
import Data.Validation.Polyform.Prim (V, Validation)
import Data.Validation.Polyform.Simple.Form (Field(Password, Input), FieldValue(FieldVal), Form(Form), Query, FieldAttrs, input, password)

-- | Unfortunatlly I've not found better interface for "form component" and
-- | if you want to combine results from multiple such components
-- | it requires a little work on your side.
-- | In general you should be able to combine `init` functions
-- | by just using monoidal append on them.
-- | Validation can be combined into product by using applicative instance.

newtype FormComponent field m a = FormComponent
  { --  init ∷ Op (m (Form field)) (Maybe a)
    init ∷ (Maybe a) → m (Form field)
  -- | ^ we are getting `Divisible` instance from `Op` which allows us to compose smaller
  -- | initializers into large ones
  , validation ∷ Validation m (Form field) Query a
  }
derive instance newtypeFormComponent ∷ Newtype (FormComponent f m a) _
derive instance genericFormComponent ∷ Generic (FormComponent f m a) _

instance invariantFunctorFormComponent ∷ (Functor m) ⇒ Invariant (FormComponent field m) where
  imap f g (FormComponent { init, validation }) = FormComponent { init: init <<< (g <$> _), validation: f <$> validation }

-- | I'm not sure if this instance is useful as you usually want to aggregate data into custom datatype and not in a
-- | monoidal structure ;-)
instance semigroupFormComponent ∷ (Semigroup a, Semigroup (m (Form field)), Semigroup (m (V (Form field) a))) ⇒ Semigroup (FormComponent field m a) where
  append (FormComponent fc1) (FormComponent fc2) =
    FormComponent { init: (fc1.init <> fc2.init), validation: fc1.validation <> fc2.validation }

inputComponent ∷ ∀ m. (Monad m) ⇒ m (Record (FieldAttrs ())) → FormComponent Field m String
inputComponent mAttrs =
  FormComponent { validation, init }
 where
  validation = input mAttrs
  init s = do
    { id, label, name } ← mAttrs
    pure (Form empty [ Input { id, label, name, value: FieldVal s }])

passwordComponent ∷ ∀ m. (Monad m) ⇒ m (Record (FieldAttrs ())) → FormComponent Field m String
passwordComponent mAttrs =
  FormComponent { validation, init }
 where
  validation = password mAttrs
  init s = do
    { id, label, name } ← mAttrs
    pure (Form empty [ Password { id, label, name, value: FieldVal s }])
