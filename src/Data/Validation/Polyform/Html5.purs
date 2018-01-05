module Data.Validation.Polyform.Html5 where

import Prelude

import Data.Array (catMaybes, foldMap)
import Data.Either (Either(..), hush, note)
import Data.Foldable (all)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (class Monoid)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (ala, alaF)
import Data.Profunctor.Choice (left, right)
import Data.String (length, toLower)
import Data.Traversable (and, sequence, traverse)
import Data.Validation.Polyform.Form as Form
import Data.Validation.Polyform.Http (HttpForm)
import Data.Validation.Polyform.Http as Http
import Data.Validation.Polyform.Validation.Field (FieldValidation(..), check, int', pureV, required', scalar', tag, validate)
import Data.Variant (Variant, inj)
import Type.Prelude (SProxy(..))


-- | RangeInput can be used to represent `type="range"`  `type="number"`
-- | for Integer values
type RangeInputErr e = Variant (min ∷ Int, max ∷ Int | e)
data RangeInputType = RangeInput | NumberInput
derive instance genericRangeInputType ∷ Generic RangeInputType _

showRangeInputType ∷ RangeInputType → String
showRangeInputType RangeInput = "range"
showRangeInputType NumberInput = "number"

type RangeInput err attrs =
  { name ∷ String
  , min ∷ Maybe Int
  , max ∷ Maybe Int
  , step ∷ Int
  , type ∷ RangeInputType
  , value ∷ Either (RangeInputErr err) Int
  | attrs
  }

rangeInput
  ∷ ∀ attrs e m
  . (Monad m)
  ⇒ { min ∷ Maybe Int, max ∷ Maybe Int | attrs }
  → FieldValidation m (RangeInputErr e) Int Int
rangeInput r =
  minV <<< maxV
 where
  maxV = tag (SProxy ∷ SProxy "max") (check (\i → maybe true (i <= _) r.max))
  minV = tag (SProxy ∷ SProxy "min") (check (\i → maybe true (i >= _) r.min))

_rangeInput = (SProxy ∷ SProxy "rangeInput")

rangeForm
  ∷ ∀ attrs err form m o
  . Monad m
  ⇒ Monoid form
  ⇒ (Variant (rangeInput ∷ RangeInput (int ∷ String, scalar ∷ Array String, required ∷ Unit | err) attrs | o) → form)
  → RangeInput (int ∷ String, scalar ∷ Array String, required ∷ Unit | err) attrs
  → HttpForm m form Int
rangeForm singleton field =
  let
    validation = rangeInput field <<< int' <<< scalar' <<< required' <<< pureV catMaybes
  in
    Http.inputForm
      (\field → singleton (inj _rangeInput field))
      field
      validation


-- | All these input types share same attributes... but email.
-- | Email has additional "multiple" attribute
-- | but this will be handled by separate field for handling list of
-- | emails.
data TextInputType = SearchInput | TelInput | TextInput | UrlInput | EmailInput
derive instance genericTextInputType ∷ Generic TextInputType _

showInputType ∷ TextInputType → String
showInputType SearchInput = "search"
showInputType TelInput = "tel"
showInputType TextInput = "text"
showInputType UrlInput = "url"
showInputType EmailInput = "email"

type TextInputErr err =
  Variant (maxlength ∷ String, minlength ∷ String | err)

type TextInputBase r err attrs =
  { name ∷ String
  , maxlength ∷ Maybe Int
  , minlength ∷ Maybe Int
  , value ∷ Either (TextInputErr err) r
  , type ∷ TextInputType
  | attrs
  }
type TextInput err attrs = TextInputBase String err attrs
type OptTextInput err attrs = TextInputBase (Maybe String) err attrs

textInput
  ∷ ∀ attrs err m
  . Monad m
  ⇒ { maxlength ∷ Maybe Int, minlength ∷ Maybe Int | attrs }
  → FieldValidation m (TextInputErr err) String String
textInput r =
  maxV >>> minV
 where
  maxV = tag (SProxy ∷ SProxy "maxlength") (check (\i → maybe true (length i > _) r.maxlength))
  minV = tag (SProxy ∷ SProxy "minlength") (check (\i → maybe true (length i < _) r.minlength))

_textInput = SProxy ∷ SProxy "textInput"

textInputForm
  ∷ ∀ attrs err form m o
  . Monad m
  ⇒ Monoid form
  ⇒ (Variant (textInput ∷ TextInput (scalar ∷ Array String, required ∷ Unit | err) attrs | o) → form)
  → TextInput (scalar ∷ Array String, required ∷ Unit | err) attrs
  → HttpForm m form String
textInputForm singleton field =
  let
    validation = textInput field <<< scalar' <<< required' <<< pureV catMaybes
  in
    Http.inputForm
      (\field → singleton (inj _textInput field))
      field
      validation


type PasswordInput err attrs =
  { name ∷ String
  -- , inputmode ∷ XXX
  , maxlength ∷ Maybe Int
  , minlength ∷ Maybe Int
  , value ∷ Either (TextInputErr err) String
  | attrs
  }

_passwordInput = SProxy ∷ SProxy "passwordInput"

passwordInputForm
  ∷ ∀ attrs err form m o
  . Monad m
  ⇒ Monoid form
  ⇒ (Variant (passwordInput ∷ PasswordInput (scalar ∷ Array String, required ∷ Unit | err) attrs | o) → form)
  → PasswordInput (required ∷ Unit, scalar ∷ Array String | err) attrs
  → HttpForm m form String
passwordInputForm singleton field =
  let
    validation = textInput field <<< scalar' <<< required' <<< pureV catMaybes
  in
    Http.inputForm
      (\field → singleton (inj _passwordInput field))
      field
      validation
