module Polyform.Input.Http where

import Prelude

import Data.Array (catMaybes, singleton)
import Data.Maybe (Maybe, fromMaybe)
import Data.NonEmpty (NonEmpty)
import Data.Profunctor (lcmap)
import Foreign.Object (Object, lookup)
import Data.Variant (Variant)
import Polyform.Field.Html5 (IntInputErr, TextInputErr)
import Polyform.Field.Html5 as Html5
import Polyform.Field.Validation.Combinators (int, required, scalar)
import Polyform.Form.Component as Form.Component
import Polyform.Validation (V, Validation, hoistFn)

-- | This representation should cover all
-- | possible http query values:
-- | `?field`, `?field=`,
-- | `?field=value`,
-- | `?field=value1&field=value2`
type Value = Array (Maybe String)
type Query = Object Value

type StringErr e = (scalar ∷ NonEmpty Array String, required ∷ Unit | e)
type OptStringErr e = (scalar ∷ NonEmpty Array String | e)

type EmailInput attrs err = Html5.EmailInput attrs String (StringErr err)
type OptEmailInput attrs err = Html5.OptEmailInput attrs String (OptStringErr err)

type SearchInput attrs err = Html5.SearchInput attrs String (StringErr err)
type OptSearchInput attrs err = Html5.OptSearchInput attrs String (OptStringErr err)

type PasswordInput attrs err = Html5.PasswordInput attrs String (StringErr err)
type OptPasswordInput attrs err = Html5.OptPasswordInput attrs String (OptStringErr err)

type TelInput attrs err = Html5.TelInput attrs String (StringErr err)
type OptTelInput attrs err = Html5.OptTelInput attrs String (OptStringErr err)

type TextInput attrs err = Html5.TextInput attrs String (StringErr err)
type OptTextInput attrs err = Html5.OptTextInput attrs String (OptStringErr err)

type UrlInput attrs err = Html5.UrlInput attrs String (StringErr err)
type OptUrlInput attrs err = Html5.OptUrlInput attrs String (OptStringErr err)

textInputValidation
  ∷ ∀ attrs err m
  . Monad m
  ⇒ { minlength ∷ Maybe Int, maxlength ∷ Maybe Int | attrs }
  → Validation m (Array (Variant (TextInputErr (StringErr err)))) Value String
textInputValidation r =
  hoistFn catMaybes >>> required singleton >>> scalar singleton >>> Html5.textInputValidation r

type IntErr err = StringErr (int ∷ String | err)

intInputValidation
  ∷ ∀ attrs err m
  . Monad m
  ⇒ { min ∷ Maybe Int, max ∷ Maybe Int | attrs }
  → Validation m (Array (Variant (IntInputErr (IntErr err)))) Value Int
intInputValidation r
  = hoistFn catMaybes
  >>> required singleton
  >>> scalar singleton
  >>> int singleton
  >>> Html5.numberInputValidation r

-- | XXX: Drop this "coerce" version
fromFieldCoerce
  ∷ ∀ attrs e form m v v'
  . Monad m
  ⇒ Monoid e
  ⇒ (v → v')
  → ({ value ∷ V e v, name ∷ String | attrs } → form)
  → { value ∷ V e v , name ∷ String | attrs }
  → Validation m e Value v
  → Form.Component.Component m form Query v'
fromFieldCoerce coerce singleton field validation =
  Form.Component.fromFieldCoerce coerce singleton field (lcmap fieldQuery validation)
 where
  fieldQuery query = fromMaybe [] (lookup field.name query)

fromField
  ∷ ∀ attrs e form m v
  . Monad m
  ⇒ Monoid e
  ⇒ ({ value ∷ V e v, name ∷ String | attrs } → form)
  → { value ∷ V e v, name ∷ String | attrs }
  → Validation m e Value v
  → Form.Component.Component m form Query v
fromField = fromFieldCoerce identity

