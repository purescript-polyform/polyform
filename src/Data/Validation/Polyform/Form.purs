module Data.Validation.Polyform.Form where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List(..), singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Record (insert, set)
import Data.Tuple (Tuple(..))
import Data.Validation.Polyform.Field (class Choices, class Options, ChoiceField, MultiChoiceField, choicesParser, options, optionsParser)
import Data.Validation.Polyform.Field.Option (Option)
import Data.Validation.Polyform.Field.Option as SymbolOption
import Data.Validation.Polyform.Validation.Field (FieldValidation, runFieldValidation, tag)
import Data.Validation.Polyform.Validation.Form (V(..), Validation(..), bimapResult)
import Data.Variant (Variant, inj)
import Type.Prelude (class IsSymbol, Proxy(..), SProxy(SProxy))

-- | This module provides some helpers for building basic HTML forms.
-- |
-- | All builders operate on really trivial form type
-- | `type Form field = List field`, because we want to
-- | ease the transition between `FieldValidation` and `Validation`.

type Form field = List field

-- (a → ((e → r) → (v → r) → m r))

inputForm
  ∷ ∀ attrs e m q v
  . Monad m
  ⇒ Record (value ∷ Either e v | attrs)
  → FieldValidation m e q v
  → Validation m (Form (Record (value ∷ Either e v | attrs))) (Maybe q) (Maybe v)
inputForm field validation = Validation $
  case _ of
    Just query' → do
      r ← runExceptT (runFieldValidation validation query')
      pure $ case r of
        Left e → Invalid (singleton $ field { value = Left e })
        Right v → Valid (singleton $ field { value = Right v }) (Just v)
    Nothing → do
      pure $ Valid (singleton field) Nothing

inputForm'
  ∷ ∀ attrs e n m o o' q v
  . Monad m
  ⇒ RowCons n (Record (value ∷ Either e v | attrs)) o o'
  ⇒ IsSymbol n
  ⇒ SProxy n
  → Record (value ∷ Either e v | attrs)
  → FieldValidation m e q v
  → Validation m (Form (Variant o')) (Maybe q) (Maybe v)
inputForm' p r =
  bimapResult (inj p <$> _) id <<< inputForm r

_invalidOption = SProxy ∷ SProxy "invalidOption"

-- coproductChoiceForm ∷ ∀ a attrs e q rep m
--   . Monad m
--   ⇒ Generic a rep
--   ⇒ ChoiceField e opt attrs
--   → FieldValidation m (Variant (invalidOption ∷ String | e)) q String
--   → Validation m (List (ChoiceField (Variant (invalidOption ∷ String | e)) a attrs)) (Maybe q) (Maybe a)
-- coproductChoiceForm field validation = Validation
--   let
--     _p = Proxy ∷ Proxy a
--     opts = fromMaybe (options _p) field.options
--     field' = set (SProxy ∷ SProxy "options") opts field
--   in
--     case _ of
--       Nothing → pure $ Valid (singleton field') Nothing
--       Just query → do
--         let
--           optsValidation = tag _invalidOption $ optionsParser _p
--           validation' = optsValidation <<< validation
--         r ← runExceptT (runFieldValidation validation' query)
--         pure $ case r of
--           Left e → Invalid (singleton (field' { value = Left e }))
--           Right v → Valid (singleton (field' { value = Right v })) (Just v)
-- 
-- symbolChoiceForm ∷ ∀ a e i m
--   . Monad m
--   ⇒ Options (Option a)
--   ⇒ ChoiceField (Variant (invalidOption ∷ String | e)) (Option a)
--   → Proxy a
--   → FieldValidation m (Variant (invalidOption ∷ String | e)) i String
--   → Validation m (Form (ChoiceField (Variant (invalidOption ∷ String | e)) (Option a))) (Maybe i) (Maybe (Option a))
-- symbolChoiceForm field validation = Validation
--   let
--     p = Proxy ∷ Proxy a
--     field' =
--       case field.options of
--         Nil → field { options = options p }
--         otherwise → field
--   in
--     case _ of
--       Nothing → pure $ Valid (singleton field') Nothing
--       Just query → do
--         let
--           optsValidation = tag _invalidOption $ SymbolOption.optionsParser p
--           validation' = optsValidation <<< validation
--         r ← runExceptT (runFieldValidation validation' query)
--         pure $ case r of
--           Left e → Invalid (singleton (field' { value = Left e }))
--           Right v → Valid (singleton (field' { value = Right v })) (Just v)

-- coproductMultiChoiceForm ∷ ∀ a e i rep row m
--   . Monad m
--   ⇒ Generic a rep
--   ⇒ Choices rep row
--   ⇒ Options rep
--   ⇒ String
--   → Proxy a
--   → FieldValidation m (Variant (invalidOption ∷ String | e)) i (Array String)
--   → Validation m (Form (MultiChoice (Variant (invalidOption ∷ String | e)) a)) i (Record row)
-- coproductMultiChoiceForm name p validation = Validation $ \query → do
--   let
--     validation' = validation >>> (tag _invalidOption $ choicesParser p)
--     opts = options p
--   r ← runExceptT (runFieldValidation validation' query)
--   pure $ case r of
--     Left e → Invalid (singleton {name, choices: opts, value: Left e})
--     Right { product, checkOpt } → Valid (singleton {name, choices: opts, value: Right checkOpt}) product
-- 
-- symbolMultiChoiceForm ∷ ∀ a e i row m
--   . Monad m
--   ⇒ Choices (Option a) row
--   ⇒ Options (Option a)
--   ⇒ String
--   → Proxy a
--   → FieldValidation m (Variant (invalidOption ∷ String | e)) i (Array String)
--   → Validation m (Form (MultiChoice (Variant (invalidOption ∷ String | e)) (Option a))) i (Record row)
-- symbolMultiChoiceForm name p validation = Validation $ \query → do
--   let
--     validation' = validation >>> (tag _invalidOption $ SymbolOption.choicesParser p)
--     opts = SymbolOption.options p
--   r ← runExceptT (runFieldValidation validation' query)
--   pure $ case r of
--     Left e → Invalid (singleton { name, choices: opts, value: Left e })
--     Right { product, checkOpt } → Valid (singleton { name, choices: opts, value: Right checkOpt }) product
-- 
