module Data.Validation.Polyform.Http where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Monoid (class Monoid)
import Data.StrMap (StrMap, lookup)
import Data.Validation.Polyform.Field (InputField)
import Data.Validation.Polyform.Form as Form
import Data.Validation.Polyform.Validation.Field as FieldValidation
import Data.Validation.Polyform.Validation.Form (bimapResult)
import Data.Validation.Polyform.Validation.Form as FormValidation
import Data.Variant (Variant, inj)
import Type.Prelude (class IsSymbol, SProxy)

type HttpFieldQuery = Array (Maybe String)
type HttpQuery = StrMap HttpFieldQuery

type HttpFormValidation m e a = FormValidation.Validation m e (Maybe HttpQuery) a
type HttpFieldValidation m e a = FieldValidation.FieldValidation m e (Maybe HttpFieldQuery) a

inputForm
  ∷ ∀ attrs e m v
  . Monad m
  ⇒ Record (name ∷ String, value ∷ Either e v | attrs)
  → FieldValidation.FieldValidation m e HttpFieldQuery v
  → HttpFormValidation m (Form.Form (Record (name ∷ String, value ∷ Either e v | attrs))) (Maybe v)
inputForm field validation =
  fieldQuery field.name >>> Form.inputForm field validation

fieldQuery ∷ ∀ l m. Monad m ⇒ Monoid l ⇒ String → HttpFormValidation m l (Maybe HttpFieldQuery)
fieldQuery name = FormValidation.pureV \q → do
  q' ← q
  lookup name q' <|> pure []

inputForm'
  ∷ ∀ attrs e m n o o' v
  . Monad m
  ⇒ RowCons n (Record (name ∷ String, value ∷ Either e v | attrs)) o' o
  ⇒ IsSymbol n
  ⇒ SProxy n
  → InputField e v attrs
  → FieldValidation.FieldValidation m e HttpFieldQuery v
  → HttpFormValidation m (Form.Form (Variant o)) (Maybe v)
inputForm' p field =
  bimapResult (inj p <$> _) id <<< inputForm field

-- coproductChoiceForm
--   ∷ ∀ a e rep row m
--   . Monad m
--   ⇒ Generic a rep
--   ⇒ Options rep
--   ⇒ Record (name ∷ String, value ∷ Either e opt)
--   → a
--   → HttpFormValidation m (Form.Form (ChoiceField (Variant (invalidOption ∷ String, scalar ∷ Array String | e)) a)) (Maybe a)
-- coproductChoiceForm name p =
--   let
--     scalar = FieldValidation.pureV catMaybes >>> FieldValidation.scalar'
--   in
--     fieldQuery name >>> Form.coproductChoiceForm name p scalar

-- symbolChoiceForm ∷ ∀ a e rep row m
--   . Monad m
--   ⇒ Options (Option a)
--   ⇒ String
--   → Proxy a
--   → HttpFormValidation m (Form.Form (Choice (Variant (invalidOption ∷ String, scalar ∷ Array String | e)) (Option a))) (Option a)
-- symbolChoiceForm name p =
--   let
--     scalar = FieldValidation.pureV catMaybes >>> FieldValidation.scalar'
--   in
--     FormValidation.pureV (\query → fromMaybe [] (lookup name query)) >>> Form.symbolChoiceForm name p scalar

-- coproductMultiChoiceForm ∷ ∀ a e rep row m
--   . Monad m
--   ⇒ Generic a rep
--   ⇒ Choices rep row
--   ⇒ Options rep
--   ⇒ String
--   → Proxy a
--   → HttpFormValidation m (Form.Form (MultiChoice (Variant (required ∷ Unit, invalidOption ∷ String | e)) a)) (Record row)
-- coproductMultiChoiceForm name p =
--   let
--     required = FieldValidation.pureV catMaybes >>> FieldValidation.required'
--   in
--     FormValidation.pureV (\query → fromMaybe [] (lookup name query)) >>> Form.coproductMultiChoiceForm name p required

-- symbolMultiChoiceForm ∷ ∀ a e rep row m
--   . Monad m
--   ⇒ Choices (Option a) row
--   ⇒ Options (Option a)
--   ⇒ String
--   → Option a
--   → HttpFormValidation m (Form.Form (MultiChoice (Variant (required ∷ Unit, invalidOption ∷ String | e)) (Option a))) (Record row)
-- symbolMultiChoiceForm name p =
--   let
--     required = FieldValidation.pureV catMaybes >>> FieldValidation.required'
--   in
--     FormValidation.pureV (\query → fromMaybe [] (lookup name query)) >>> Form.symbolMultiChoiceForm name p required
