module Polyform.Input.Http where

import Prelude

import Data.Array (catMaybes)
import Data.Either (Either)
import Data.Maybe (Maybe, fromMaybe)
import Data.Profunctor (lmap)
import Data.StrMap (StrMap, lookup)
import Data.Variant (Variant)
import Polyform.Field as Field
import Polyform.Field.Html5 as Html5
import Polyform.Field.Validation (liftPure, required, scalar)
import Polyform.Input.Interpret.Http (StringErr)
import Polyform.Form.Component as Form.Component

-- | This representation should cover all
-- | possible http query values:
-- | `?field`, `?field=`,
-- | `?field=value`,
-- | `?field=value1&field=value2`
type Value = Array (Maybe String)
type Query = StrMap Value

type TextInputErr err = Html5.TextInputErr (StringErr err)

type EmailInput attrs err = Html5.EmailInput attrs String (TextInputErr err)
type OptEmailInput attrs err = Html5.OptEmailInput attrs String (TextInputErr err)

type PasswordInput attrs err = Html5.PasswordInput attrs String (TextInputErr err)
type OptPasswordInput attrs err = Html5.OptPasswordInput attrs String (TextInputErr err)

type SearchInput attrs err = Html5.SearchInput attrs String (TextInputErr err)
type OptSearchInput attrs err = Html5.OptSearchInput attrs String (TextInputErr err)

type TelInput attrs err = Html5.TelInput attrs String (TextInputErr err)
type OptTelInput attrs err = Html5.OptTelInput attrs String (TextInputErr err)

type TextInput attrs err = Html5.TextInput attrs String (TextInputErr err)
type OptTextInput attrs err = Html5.OptTextInput attrs String (TextInputErr err)

type UrlInput attrs err = Html5.UrlInput attrs String (TextInputErr err)
type OptUrlInput attrs err = Html5.OptUrlInput attrs String (TextInputErr err)

textInputValidation
  ∷ ∀ attrs err m
  . Monad m
  ⇒ { maxlength ∷ Maybe Int, minlength ∷ Maybe Int | attrs }
  → Field.Validation m (Variant (TextInputErr err)) Value String
textInputValidation r =
  liftPure catMaybes >>> required >>> scalar >>> Html5.textInputValidation r

fromField
  ∷ ∀ attrs e form m v
  . Monad m
  ⇒ ({ value ∷ Either e v, name ∷ String | attrs } -> form)
  → { value ∷ Either e v , name ∷ String | attrs }
  → Field.Validation m e Value v
  → Form.Component.Component m form Query v
fromField singleton field validation =
  Form.Component.fromField singleton field (lmap fieldQuery validation)
 where
  fieldQuery query = fromMaybe [] (lookup field.name query)

