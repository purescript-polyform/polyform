module Data.Validation.Polyform.Form where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List, singleton)
import Data.Maybe (Maybe(..))
import Data.Validation.Polyform.Field (class Choices, class Options, Choice, MultiChoice, choicesParser, options, optionsParser)
import Data.Validation.Polyform.Field.Option (Option)
import Data.Validation.Polyform.Field.Option as SymbolOption
import Data.Validation.Polyform.Validation.Field (FieldValidation, runFieldValidation, tag)
import Data.Validation.Polyform.Validation.Form (V(..), Validation(..))
import Data.Variant (Variant)
import Type.Prelude (Proxy, SProxy(SProxy))

-- | This module provides some helpers for building basic HTML forms.
-- |
-- | All builders operate on really trivial form type
-- | `type Form field = List field`, because we want to
-- | ease the transition between `FieldValidation` and `Validation`.

type Form field = List field

inputForm
  ∷ ∀ attrs e m q v
  . Monad m
  ⇒ Record (value ∷ Either e v | attrs)
  → FieldValidation m e q v
  → Validation m (Form (Record (value ∷ Either e v | attrs))) (Maybe q) v
inputForm field validation = Validation $ \query → do
  r ← case query of
    Just query' → do
      runExceptT (runFieldValidation validation query')
    Nothing → do
      pure field.value
  pure $ case r of
    Left e → Invalid (singleton $ field { value = Left e })
    Right v → Valid (singleton $ field { value = Right v }) v


_invalidOption = SProxy ∷ SProxy "invalidOption"

coproductChoiceForm ∷ ∀ a e i rep m
  . Monad m
  ⇒ Generic a rep
  ⇒ Options rep
  ⇒ String
  → Proxy a
  → FieldValidation m (Variant (invalidOption ∷ String | e)) i String
  → Validation m (Form (Choice (Variant (invalidOption ∷ String | e)) a)) i a
coproductChoiceForm name p validation = Validation $ \query → do
  let
    optsValidation = tag _invalidOption $ optionsParser p
    validation' = optsValidation <<< validation
    opts = options p
  r ← runExceptT (runFieldValidation validation' query)
  pure $ case r of
    Left e → Invalid (singleton {name, options: opts, value: Left e})
    Right v → Valid (singleton {name, options: opts, value: Right v}) v

symbolChoiceForm ∷ ∀ a e i m
  . Monad m
  ⇒ Options (Option a)
  ⇒ String
  → Proxy a
  → FieldValidation m (Variant (invalidOption ∷ String | e)) i String
  → Validation m (Form (Choice (Variant (invalidOption ∷ String | e)) (Option a))) i (Option a)
symbolChoiceForm name p validation = Validation $ \query → do
  let
    optsValidation = tag _invalidOption $ SymbolOption.optionsParser p
    validation' = optsValidation <<< validation
    opts = SymbolOption.options p
  r ← runExceptT (runFieldValidation validation' query)
  pure $ case r of
    Left e → Invalid (singleton {name, options: opts, value: Left e})
    Right v → Valid (singleton {name, options: opts, value: Right v}) v

coproductMultiChoiceForm ∷ ∀ a e i rep row m
  . Monad m
  ⇒ Generic a rep
  ⇒ Choices rep row
  ⇒ Options rep
  ⇒ String
  → Proxy a
  → FieldValidation m (Variant (invalidOption ∷ String | e)) i (Array String)
  → Validation m (Form (MultiChoice (Variant (invalidOption ∷ String | e)) a)) i (Record row)
coproductMultiChoiceForm name p validation = Validation $ \query → do
  let
    validation' = validation >>> (tag _invalidOption $ choicesParser p)
    opts = options p
  r ← runExceptT (runFieldValidation validation' query)
  pure $ case r of
    Left e → Invalid (singleton {name, choices: opts, value: Left e})
    Right { product, checkOpt } → Valid (singleton {name, choices: opts, value: Right checkOpt}) product

symbolMultiChoiceForm ∷ ∀ a e i row m
  . Monad m
  ⇒ Choices (Option a) row
  ⇒ Options (Option a)
  ⇒ String
  → Proxy a
  → FieldValidation m (Variant (invalidOption ∷ String | e)) i (Array String)
  → Validation m (Form (MultiChoice (Variant (invalidOption ∷ String | e)) (Option a))) i (Record row)
symbolMultiChoiceForm name p validation = Validation $ \query → do
  let
    validation' = validation >>> (tag _invalidOption $ SymbolOption.choicesParser p)
    opts = SymbolOption.options p
  r ← runExceptT (runFieldValidation validation' query)
  pure $ case r of
    Left e → Invalid (singleton { name, choices: opts, value: Left e })
    Right { product, checkOpt } → Valid (singleton { name, choices: opts, value: Right checkOpt }) product

