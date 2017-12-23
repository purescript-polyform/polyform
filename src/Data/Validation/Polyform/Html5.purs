module Data.Validation.Polyform.Html5 where

import Prelude

import Data.Array (catMaybes, foldMap)
import Data.Either (Either(..), hush, note)
import Data.Foldable (all)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (ala, alaF)
import Data.Profunctor.Choice (left, right)
import Data.String (length, toLower)
import Data.Traversable (and, sequence, traverse)
import Data.Validation.Polyform.Http (HttpFormValidation)
import Data.Validation.Polyform.Http as Http
import Data.Validation.Polyform.Validation.Field (FieldValidation(..), check, int', pureV, scalar', tag, validate)
import Data.Validation.Polyform.Validation.Form (bimapResult)
import Data.Validation.Polyform.Validation.Form as Form
import Data.Variant (Variant, inj)
import Text.Smolder.HTML.Attributes (maxlength)
import Type.Prelude (SProxy(..))

-- | Range can be used to represent `type="range"`  `type="number"`
-- | for Integer values
type RangeErr e = Variant (min ∷ Int, max ∷ Int | e)
data RangeType = Range | Number
derive instance genericRangeType ∷ Generic RangeType _

showRangeType ∷ RangeType → String
showRangeType = toLower <<< genericShow

type Range err attrs =
  { name ∷ String
  , min ∷ Maybe Int
  , max ∷ Maybe Int
  , step ∷ Int
  , type ∷ RangeType
  , value ∷ Either (RangeErr err) Int
  | attrs
  }

range
  ∷ ∀ attrs e m
  . (Monad m)
  ⇒ { min ∷ Maybe Int, max ∷ Maybe Int | attrs }
  → FieldValidation m (RangeErr e) Int Int
range r =
  minV <<< maxV
 where
  maxV = tag (SProxy ∷ SProxy "max") (check (\i → maybe true (i > _) r.max))
  minV = tag (SProxy ∷ SProxy "min") (check (\i → maybe true (i < _) r.min))

_range = (SProxy ∷ SProxy "range")

rangeForm
  ∷ ∀ attrs err m o
  . (Monad m)
  ⇒ Range (int ∷ String, scalar ∷ Array String | err) attrs
  → HttpFormValidation m (List (Variant (range ∷ Range (int ∷ String, scalar ∷ Array String | err) attrs | o ))) (Maybe Int)
rangeForm field =
  let
    validation = range field <<< int' <<< scalar' <<< pureV catMaybes
  in
    bimapResult (inj _range <$> _) id $ Http.inputForm field validation


-- | All these input types share same attributes... but email.
-- | Email has additional "multiple" attribute
-- | but this will be handled by separate field for handling list of
-- | emails.
data TextInputType = SearchInput | TelInput | TextInput | UrlInput | EmailInput
derive instance genericTextInputType ∷ Generic TextInputType _

showInputType ∷ TextInputType → String
showInputType = toLower <<< genericShow

type TextInputErr err =
  Variant (maxlength ∷ String, minlength ∷ String | err)

type TextInput err attrs =
  { name ∷ String
  , maxlength ∷ Maybe Int
  , minlength ∷ Maybe Int
  , value ∷ Either (TextInputErr err) String
  , type ∷ TextInputType
  | attrs
  }

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
  ∷ ∀ attrs err m o
  . (Monad m)
  ⇒ TextInput (scalar ∷ Array String | err) attrs
  → HttpFormValidation m (List (Variant (textInput ∷ TextInput (scalar ∷ Array String | err) attrs | o))) (Maybe String)
textInputForm field =
  let
    validation = textInput field <<< scalar' <<< pureV catMaybes
  in
    bimapResult (inj _textInput <$> _) id $ Http.inputForm field validation


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
  ∷ ∀ attrs err m o
  . (Monad m)
  ⇒ PasswordInput (scalar ∷ Array String | err) attrs
  → HttpFormValidation m (List (Variant (passwordInput ∷ PasswordInput (scalar ∷ Array String | err) attrs | o))) (Maybe String)
passwordInputForm field =
  let
    validation = textInput field <<< scalar' <<< pureV catMaybes
  in
    bimapResult (inj _passwordInput <$> _) id $ Http.inputForm field validation
    -- Form.pureV hush <<< right (Http.inputForm field validation) <<< Form.pureV (note unit)
