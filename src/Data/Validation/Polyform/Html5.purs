module Data.Validation.Polyform.Html5 where

import Prelude

import Data.Array (catMaybes, filter, foldMap)
import Data.Either (Either(..), hush, note)
import Data.Foldable (all)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (class Monoid)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (ala, alaF)
import Data.NonEmpty (NonEmpty(..))
import Data.Profunctor.Choice (left, right)
import Data.String (length, null, toLower)
import Data.Traversable (and, sequence, traverse)
import Data.Validation.Polyform.Form (INT, IntF(..), StringF(..), STRING)
import Data.Validation.Polyform.Form as Form
import Data.Validation.Polyform.Validation.Field (FieldValidation(..), check, int', opt, pureV, required', scalar', tag, validate)
import Data.Variant (Variant, inj)
import Run (FProxy(..), Run(..))
import Text.Smolder.HTML.Attributes (maxlength)
import Type.Prelude (class IsSymbol, SProxy(..))


-- | RangeInput can be used to represent `type="range"`  `type="number"`
-- | for Integer values
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

rangeForm
  ∷ ∀ attrs eff err form name names names' o q
  . Monoid form
  ⇒ RowCons name Unit names names'
  ⇒ (IsSymbol name)
  ⇒ (RangeInput err attrs name → form)
  → RangeInput err attrs name
  → Form.Form (Run (int ∷ (INT names' (Variant (min ∷ Int, max ∷ Int | err)) q) | eff)) form q Int
rangeForm singleton field =
  Form.intForm
    singleton
    field
    validation
 where
  validation =
    minV <<< maxV
   where
    maxV = tag _max (check (\i → maybe true (i <= _) field.max))
    minV = tag _min (check (\i → maybe true (i >= _) field.min))

_optRangeInput = (SProxy ∷ SProxy "optRangeInput")

-- optRangeForm
--   ∷ ∀ attrs eff err form name o q
--   . Monoid form
--   ⇒ IsSymbol name
--   ⇒ (OptRangeInput err attrs name → form)
--   → OptRangeInput err attrs name
--   → Form.Form (Run (optInt ∷ FProxy (OptIntF name q (Variant (min ∷ Int, max ∷ Int | err))) | eff)) form q (Maybe Int)
-- optRangeForm singleton field =
--   Form.optIntForm
--     (\field' → singleton field')
--     field
--     validation
--  where
--   validation =
--     minV <<< maxV
--    where
--     maxV = tag _max (check (\i → maybe true (i <= _) field.max))
--     minV = tag _min (check (\i → maybe true (i >= _) field.min))
-- 
-- 
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
  → FieldValidation m (Variant (TextInputErr err)) String String
textInputValidation r =
  maxV >>> minV
 where
  maxV = tag _maxlength (check (\i → maybe true (length i > _) r.maxlength))
  minV = tag _minlength (check (\i → maybe true (length i < _) r.minlength))

textInputForm
  ∷ ∀ attrs eff err form m name names names' q
  . Monoid form
  ⇒ RowCons name Unit names names'
  ⇒ IsSymbol name
  ⇒ (TextInput err attrs name → form)
  → TextInput err attrs name
  → Form.Form (Run (string ∷ (STRING names' (Variant (TextInputErr err)) q) | eff)) form q String
textInputForm singleton field =
  Form.stringForm
    singleton
    field
    (textInputValidation field)
