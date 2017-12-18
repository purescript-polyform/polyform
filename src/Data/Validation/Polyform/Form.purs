module Data.Validation.Polyform.Form where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List, singleton)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.StrMap (StrMap, lookup)
import Data.Validation.Polyform.Field (class Choices, class Options, Choice, Input, MultiChoice, choicesParser, options, optionsParser)
import Data.Validation.Polyform.Field.Option (Option)
import Data.Validation.Polyform.Field.Option as SymbolOption
import Data.Validation.Polyform.Validation.Field (FieldValidation(..), nonEmpty', runFieldValidation)
import Data.Validation.Polyform.Validation.Form (V(..), Validation(..))
import Data.Variant (Variant)
import Type.Prelude (Proxy(..))

-- | This module defines only trivial form builders
-- | with single fields. `List` form representation
-- | seems to be sufficient in this context.
type Form field = List field

type FieldQuery = Array (Maybe String)
type Query = StrMap FieldQuery

type HttpFormValidation m e a = Validation m e Query a
type HttpFieldValidation m e a = FieldValidation m e FieldQuery a

inputForm ∷ ∀ e i m v. (Monad m) ⇒ String  → FieldValidation m e i v → Validation m (Form (Input e v)) i v
inputForm name validation = Validation $ \query → do
  r ← runExceptT (runFieldValidation validation query)
  pure $ case r of
    Left e → Invalid (singleton {name, value: Left e})
    Right v → Valid (singleton {name, value: Right v}) v

coproductChoiceForm ∷ ∀ a rep row m
  . (Monad m)
  ⇒ (Generic a rep)
  ⇒ (Choices rep row)
  ⇒ (Options rep)
  ⇒ String
  → Proxy a
  → Validation m (Form (MultiChoice String a row)) (Array String) (Record row)
coproductChoiceForm name p = Validation $ \query → do
  let
    validation = choicesParser p
    opts = options p
  r ← runExceptT (runFieldValidation validation query)
  pure $ case r of
    Left e → Invalid (singleton {name, choices: opts, value: Left e})
    Right v → Valid (singleton {name, choices: opts, value: Right v}) v

symbolMultipleChoiceForm ∷ ∀ a rep row m
  . (Monad m)
  ⇒ (Choices (Option a) row)
  ⇒ (Options (Option a))
  ⇒ String
  → Proxy a
  → Validation m (Form (MultiChoice String (Option a) row)) (Array String) (Record row)
symbolMultipleChoiceForm name p = Validation $ \query → do
  let
    validation = SymbolOption.choicesParser p
    opts = SymbolOption.options (Proxy ∷ Proxy (Option a))
  r ← runExceptT (runFieldValidation validation query)
  pure $ case r of
    Left e → Invalid (singleton {name, choices: opts, value: Left e})
    Right v → Valid (singleton {name, choices: opts, value: Right v}) v

-- choiceForm ∷ ∀ 

-- type Form field = List field
--
-- data Phantoms (r ∷ # Type) a = Phantoms
-- derive instance functorPhantoms ∷ Functor (Phantoms r)
--
-- instance applyPhantoms ∷ Apply (Phantoms r) where
--   apply _ _ = Phantoms
--
-- r1 = Phantoms ∷ ∀ fs. Phantoms (f1 ∷ Int | fs) Int
-- r2 = Phantoms ∷ ∀ fs. Phantoms (f2 ∷ Int | fs) Int
--
-- r3 = Phantoms ∷ ∀ fs. Phantoms (f3 ∷ Int | fs) (Int → Int → String)
--
-- r = r3 <*> r1 <*> r2
--
--
-- 
-- number name = inputValidation name (nonEmptyScalar' >>> int')
-- 
-- optNumber name = optInputValidation name (nonEmptyScalar' >>> int')
