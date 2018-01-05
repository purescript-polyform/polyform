module Data.Validation.Polyform.Http where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), either, note)
import Data.List (List, singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid)
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (left)
import Data.StrMap (StrMap, lookup)
import Data.Validation.Polyform.Field (InputField)
import Data.Validation.Polyform.Form (fromValidation)
import Data.Validation.Polyform.Form as Form
import Data.Validation.Polyform.Validation.Field (mapResult, opt)
import Data.Validation.Polyform.Validation.Field as FieldValidation
import Data.Validation.Polyform.Validation.Form (V(..), Validation(..), bimapResult, pureV)
import Data.Validation.Polyform.Validation.Form as FormValidation
import Data.Variant (SProxy(..), Variant, case_, default, inj, on)
import Type.Prelude (class IsSymbol, SProxy)

type HttpFieldQuery = Array (Maybe String)
type HttpQuery = StrMap HttpFieldQuery

type HttpForm m form a = Form.Form m form HttpQuery a
type HttpFieldValidation m e a = FieldValidation.FieldValidation m e HttpFieldQuery a

inputForm
  ∷ ∀ attrs e form m v
  . Monad m
  ⇒ Monoid form
  ⇒ (Record (name ∷ String, value ∷ Either e v | attrs) → form)
  → Record (name ∷ String, value ∷ Either e v | attrs)
  → FieldValidation.FieldValidation m e HttpFieldQuery v
  → HttpForm m form v
inputForm singleton field validation =
  fieldQuery field.name >>> Form.inputForm singleton field validation

optInputForm
  ∷ ∀ a attrs e form m v
  . Monad m
  ⇒ Monoid form
  ⇒ (Record (name ∷ String, value ∷ Either (Variant e) (Maybe v) | attrs) → form)
  → Record (name ∷ String, value ∷ Either (Variant e) (Maybe v) | attrs)
  → FieldValidation.FieldValidation m (Variant (required ∷ a | e)) HttpFieldQuery v
  → HttpForm m form (Maybe v)
optInputForm singleton field validation =
  fieldQuery field.name >>> Form.inputForm singleton field (opt validation)

fieldQuery ∷ ∀ e form m. Monoid form ⇒ Monad m ⇒ String → HttpForm m form HttpFieldQuery
fieldQuery name = fromValidation $ pureV $ \q → fromMaybe [] (lookup name q)

-- inputForm'
--   ∷ ∀ attrs e m n o o' v
--   . Monad m
--   ⇒ RowCons n (Record (name ∷ String, value ∷ Either e v | attrs)) o' o
--   ⇒ IsSymbol n
--   ⇒ SProxy n
--   → InputField e v attrs
--   → FieldValidation.FieldValidation m e HttpFieldQuery v
--   → HttpFormValidation m (Form.Form (Variant o)) (Maybe v)
-- inputForm' p field =
--   bimapResult (inj p <$> _) id <<< inputForm field
-- 
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
