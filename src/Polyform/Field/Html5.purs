module Polyform.Field.Html5 where

import Prelude

import Data.Array (singleton)
import Data.Maybe (Maybe, maybe)
import Data.String (length)
import Data.Variant (Variant)
import Polyform.Field (Input)
import Polyform.Field.Validation.Combinators (check, checkAndTag)
import Polyform.Validation (Validation)
import Type.Prelude (class IsSymbol, SProxy(SProxy))

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


-- | RangeInput can be used to represent
-- | `type="range"` and `type="number"`
-- | of Integer values
type RangeInputErr err = (min ∷ Int, max ∷ Int | err)

_min = SProxy ∷ SProxy "min"
_max = SProxy ∷ SProxy "max"

type RangeInputBase (type_ ∷ Symbol) attrs name err value =
  Input
    (min ∷ Maybe Int, max ∷ Maybe Int, step ∷ Maybe Int, type ∷ SProxy type_| attrs)
    name
    (Array (Variant (RangeInputErr err)))
    value

type RangeInput attrs name err = RangeInputBase "range" attrs name err Int
type OptRangeInput attrs name err = RangeInputBase "range" attrs name err (Maybe Int)

type NumberInput attrs name err = RangeInputBase "number" attrs name err Int
type OptNumberInput attrs name err = RangeInputBase "number" attrs name err (Maybe Int)

rangeInputValidation
  ∷ forall attrs err m
  . Monad m
  ⇒ { min ∷ Maybe Int, max ∷ Maybe Int | attrs }
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
    (maxlength ∷ Maybe Int, minlength ∷ Maybe Int, type ∷ SProxy type_ | attrs)
    name
    (Array (Variant (TextInputErr err)))
    value

-- | All these input types share same attributes... but email.
-- | Email has additional "multiple" attribute
-- | but this will be handled by separate field for handling list of
-- | emails.

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

_maxlength = SProxy ∷ SProxy "maxlength"
_minlength = SProxy ∷ SProxy "minlength"

textInputValidation
  ∷ ∀ attrs err m
  . Monad m
  ⇒ { maxlength ∷ Maybe Int, minlength ∷ Maybe Int | attrs }
  → Validation
      m
      (Array (Variant (TextInputErr err)))
      String
      String
textInputValidation r =
  maxV >>> minV
 where
  maxV = checkAndTag' _maxlength (\i → maybe true (length i > _) r.maxlength)
  minV = checkAndTag' _minlength (\i → maybe true (length i < _) r.minlength)
