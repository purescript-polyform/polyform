module Polyform.Field.Html5 where

import Prelude

import Data.Array (singleton)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, maybe)
import Data.String (length)
import Data.Variant (Variant)
import Polyform.Field (Input)
import Polyform.Field.Validation.Combinators (check, checkAndTag)
import Polyform.Validation (V, Validation)
import Type.Prelude (class IsSymbol, SProxy(SProxy), reflectSymbol)

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
  , value ∷ V (Array (RangeInputErr err)) value
  | attrs
  }

type RangeInput err attrs name = RangeInputBase err attrs (SProxy name) Int
type OptRangeInput err attrs name = RangeInputBase err attrs (SProxy name) (Maybe Int)


check'
  ∷ ∀ a m
  . Monad m
  ⇒ (a → Boolean)
  → Validation m (Array a) a a
check' = check singleton

checkAndTag'
  ∷ ∀ a e e' m n
  . Monad m
  ⇒ RowCons n a e' e
  ⇒ IsSymbol n
  ⇒ SProxy n
  → (a -> Boolean)
  → Validation m (Array (Variant e)) a a
checkAndTag' = checkAndTag singleton

rangeInputValidation
  ∷ forall attrs err name m
  . Monad m
  ⇒ RangeInput err attrs name
  → Validation
      m
      (Array (Variant (min ∷ Int, max ∷ Int | err)))
      Int
      Int
rangeInputValidation field =
  minV <<< maxV
 where
  maxV = checkAndTag' _max (\i → maybe true (i <= _) field.max)
  minV = checkAndTag' _min (\i → maybe true (i >= _) field.min)

type TextInputErr err = (maxlength ∷ String, minlength ∷ String | err)

type TextInputBase (type_ ∷ Symbol) attrs name err value =
  Input
    (maxlength ∷ Maybe Int, minlength ∷ Maybe Int | attrs)
    name
    (Array (Variant (TextInputErr err)))
    value

-- | All these input types share same attributes... but email.
-- | Email has additional "multiple" attribute
-- | but this will be handled by separate field for handling list of
-- | emails.

-- | XXX: Provide email validation
type EmailInput attrs name err = TextInputBase "email" attrs name err String
type OptEmailInput attrs name err = TextInputBase "email" attrs name err (Maybe String)

type SearchInput attrs name err = TextInputBase "search" attrs name err String
type OptSearchInput attrs name err = TextInputBase "search" attrs name err (Maybe String)

type PasswordInput attrs name err = TextInputBase "password" attrs name err String
type OptPasswordInput attrs name err = TextInputBase "password" attrs name err (Maybe String)

type TelInput attrs name err = TextInputBase "tel" attrs name err String
type OptTelInput attrs name err = TextInputBase "tel" attrs name err (Maybe String)

type TextInput attrs name err = TextInputBase "text"  attrs name err String
type OptTextInput attrs name err = TextInputBase "text" attrs name err (Maybe String)

-- | XXX: Provide url validation
type UrlInput attrs name err = TextInputBase "url" attrs name err String
type OptUrlInput attrs name err = TextInputBase "url" attrs name err (Maybe String)

textInputType
  ∷ ∀ attrs err name type_ value
  . IsSymbol type_
  ⇒ TextInputBase type_ attrs name err value
  → String
textInputType _ = reflectSymbol (SProxy ∷ SProxy type_)

_maxlength = SProxy ∷ SProxy "maxlength"
_minlength = SProxy ∷ SProxy "minlength"

textInputValidation
  ∷ ∀ attrs err m
  . Monad m
  ⇒ { maxlength ∷ Maybe Int, minlength ∷ Maybe Int | attrs }
  → Validation m (Array (Variant (TextInputErr err))) String String
textInputValidation r =
  maxV >>> minV
 where
  maxV = checkAndTag' _maxlength (\i → maybe true (length i > _) r.maxlength)
  minV = checkAndTag' _minlength (\i → maybe true (length i < _) r.minlength)
