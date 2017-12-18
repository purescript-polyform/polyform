module Data.Validation.Polyform.Http where

import Prelude

import Data.Array (catMaybes)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import Data.StrMap (StrMap, lookup)
import Data.Validation.Polyform.Field (class Choices, class Options, Choice, Input, MultiChoice)
import Data.Validation.Polyform.Field.Option (Option)
import Data.Validation.Polyform.Form as Form
import Data.Validation.Polyform.Validation.Field as FieldValidation
import Data.Validation.Polyform.Validation.Form as FormValidation
import Data.Variant (Variant)
import Type.Prelude (Proxy)

type HttpFieldQuery = Array (Maybe String)
type HttpQuery = StrMap HttpFieldQuery

type HttpFormValidation m e a = FormValidation.Validation m e HttpQuery a
type HttpFieldValidation m e a = FieldValidation.FieldValidation m e HttpFieldQuery a

inputForm
  ∷ ∀ e m v
  . (Monad m)
  ⇒ String
  → FieldValidation.FieldValidation m e (HttpFieldQuery) v
  → HttpFormValidation m (Form.Form (Input e v)) v
inputForm name validation =
  FormValidation.pureV (\query → fromMaybe [] (lookup name query)) >>> Form.inputForm name validation

coproductChoiceForm
  ∷ ∀ a e rep row m
  . Monad m
  ⇒ Generic a rep
  ⇒ Options rep
  ⇒ String
  → Proxy a
  → HttpFormValidation m (Form.Form (Choice (Variant (invalidOption ∷ String, scalar ∷ Array String | e)) a)) a
coproductChoiceForm name p =
  let
    scalar = FieldValidation.pureV catMaybes >>> FieldValidation.scalar'
  in
    FormValidation.pureV (\query → fromMaybe [] (lookup name query)) >>> Form.coproductChoiceForm name p scalar

symbolChoiceForm ∷ ∀ a e rep row m
  . Monad m
  ⇒ Options (Option a)
  ⇒ String
  → Proxy a
  → HttpFormValidation m (Form.Form (Choice (Variant (invalidOption ∷ String, scalar ∷ Array String | e)) (Option a))) (Option a)
symbolChoiceForm name p =
  let
    scalar = FieldValidation.pureV catMaybes >>> FieldValidation.scalar'
  in
    FormValidation.pureV (\query → fromMaybe [] (lookup name query)) >>> Form.symbolChoiceForm name p scalar

coproductMultiChoiceForm ∷ ∀ a e rep row m
  . Monad m
  ⇒ Generic a rep
  ⇒ Choices rep row
  ⇒ Options rep
  ⇒ String
  → Proxy a
  → HttpFormValidation m (Form.Form (MultiChoice (Variant (required ∷ Unit, invalidOption ∷ String | e)) a)) (Record row)
coproductMultiChoiceForm name p =
  let
    required = FieldValidation.pureV catMaybes >>> FieldValidation.required'
  in
    FormValidation.pureV (\query → fromMaybe [] (lookup name query)) >>> Form.coproductMultiChoiceForm name p required

symbolMultiChoiceForm ∷ ∀ a e rep row m
  . Monad m
  ⇒ Choices (Option a) row
  ⇒ Options (Option a)
  ⇒ String
  → Proxy a
  → HttpFormValidation m (Form.Form (MultiChoice (Variant (required ∷ Unit, invalidOption ∷ String | e)) (Option a))) (Record row)
symbolMultiChoiceForm name p =
  let
    required = FieldValidation.pureV catMaybes >>> FieldValidation.required'
  in
    FormValidation.pureV (\query → fromMaybe [] (lookup name query)) >>> Form.symbolMultiChoiceForm name p required
