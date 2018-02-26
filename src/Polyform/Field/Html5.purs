module Polyform.Field.Html5 where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, maybe)
import Data.String (length)
import Data.Variant (Variant)
import Polyform.Field.Validation (checkPure, tag, Validation)
import Type.Prelude (SProxy(..))

-- | RangeInput can be used to represent
-- | `type="range"` and `type="number"`
-- | of Integer values
type RangeInputErr e = Variant (min ∷ Int, max ∷ Int | e)
data RangeInputType = RangeInput | NumberInput
derive instance genericRangeInputType ∷ Generic RangeInputType _

showRangeInputType ∷ RangeInputType → String
showRangeInputType RangeInput = "range"
showRangeInputType NumberInput = "number"

_min = SProxy ∷ SProxy "min"
_max = SProxy ∷ SProxy "max"

type RangeInputBase err attrs name value =
  { name ∷ name
  , min ∷ Maybe Int
  , max ∷ Maybe Int
  , step ∷ Int
  , type ∷ RangeInputType
  , value ∷ Either (RangeInputErr err) value
  | attrs
  }

type RangeInput err attrs name = RangeInputBase err attrs (SProxy name) Int
type OptRangeInput err attrs name = RangeInputBase err attrs (SProxy name) (Maybe Int)

rangeInputValidation
  ∷ forall attrs err name m
  . Monad m
  ⇒ RangeInput err attrs name
  → Validation
      m
      (Variant (min ∷ Int, max ∷ Int | err))
      Int
      Int
rangeInputValidation field =
  minV <<< maxV
 where
  maxV = tag _max (checkPure (\i → maybe true (i <= _) field.max))
  minV = tag _min (checkPure (\i → maybe true (i >= _) field.min))


-- | All these input types share same attributes... but email.
-- | Email has additional "multiple" attribute
-- | but this will be handled by separate field for handling list of
-- | emails.
data TextInputType = SearchInput | TelInput | TextInput | UrlInput | EmailInput | PasswordInput
derive instance genericTextInputType ∷ Generic TextInputType _

showInputType ∷ TextInputType → String
showInputType SearchInput = "search"
showInputType TelInput = "tel"
showInputType TextInput = "text"
showInputType UrlInput = "url"
showInputType EmailInput = "email"
showInputType PasswordInput = "password"

type TextInputErr err = (maxlength ∷ String, minlength ∷ String | err)

type TextInputBase err attrs name value =
  { name ∷ name
  , maxlength ∷ Maybe Int
  , minlength ∷ Maybe Int
  , value ∷ Either (Variant (TextInputErr err)) value
  , type ∷ TextInputType
  | attrs
  }
type TextInput err attrs name = TextInputBase err attrs (SProxy name) String
type OptTextInput err attrs name = TextInputBase err attrs (SProxy name) (Maybe String)

_maxlength = SProxy ∷ SProxy "maxlength"
_minlength = SProxy ∷ SProxy "minlength"

textInputValidation
  ∷ ∀ attrs err m
  . Monad m
  ⇒ { maxlength ∷ Maybe Int, minlength ∷ Maybe Int | attrs }
  → Validation m (Variant (TextInputErr err)) String String
textInputValidation r =
  maxV >>> minV
 where
  maxV = tag _maxlength (checkPure (\i → maybe true (length i > _) r.maxlength))
  minV = tag _minlength (checkPure (\i → maybe true (length i < _) r.minlength))
