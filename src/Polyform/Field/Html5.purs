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


-- | NumberInput can be used to represent
-- | `type="range"` and `type="number"`
-- | of Integer values
type IntInputErr err = (min ∷ Int, max ∷ Int | err)

_min = SProxy ∷ SProxy "min"
_max = SProxy ∷ SProxy "max"

-- | I'm not sure if this `type ∷ SProxy` attribute
-- | is really good idea.
-- | If you find it problematic please fill an issue.
type NumberInputBase (type_ ∷ Symbol) attrs name err value f =
  Input
    (min ∷ Maybe value, max ∷ Maybe value, step ∷ Maybe value, type ∷ SProxy type_| attrs)
    name
    (Array (Variant err))
    (f value)

type I a = a

type IntRangeInput attrs name err = NumberInputBase "range" attrs name (IntInputErr err) Int I
type OptIntRangeInput attrs name err = NumberInputBase "range" attrs name err Int Maybe

type IntInput attrs name err = NumberInputBase "number" attrs name (IntInputErr err) Int I
type OptIntInput attrs name err = NumberInputBase "number" attrs name err Int Maybe

numberInputValidation
  ∷ forall attrs err m v
  . Monad m
  ⇒ Ord v
  ⇒ { min ∷ Maybe v, max ∷ Maybe v | attrs }
  → Validation
      m
      (Array (Variant (min ∷ v, max ∷ v | err)))
      v
      v
numberInputValidation field =
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
