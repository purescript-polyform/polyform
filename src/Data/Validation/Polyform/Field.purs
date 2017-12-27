module Data.Validation.Polyform.Field where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader.Class (ask)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.List (List, singleton)
import Data.Newtype (unwrap)
import Data.Record (insert)
import Data.Tuple (Tuple(Tuple))
import Data.Validation.Polyform.Field (class Choices, class Options, choicesParserImpl, optionsImpl, optionsParserImpl, toOptionValueImpl)
import Data.Validation.Polyform.Validation.Field (FieldValidation, Last(Last), pureV, validate, withException)
import Type.Prelude (class IsSymbol, class RowLacks, Proxy(..), SProxy(..), reflectSymbol)


-- | This module provides some very simple representations of HTML fields.
-- | Don't look for single sum type or Variant which ties all of them.
-- | They are not implemented here.
-- |
-- | Fields represantation is as minimal as possible, so only
-- | validation relevant pieces are exposed as attrs.

type InputField e a attrs =
  { name ∷ String
  , value ∷ Either e a
  | attrs
  }

type ChoiceField e opt attrs opts =
  { name ∷ String
  , options ∷ List (Tuple String opt)
  , value ∷ Either e opt
  | attrs
  }

-- | This field can be used to represent multiple checkboxes with the same
-- | `name` or `select` with `multiple` - it's a product type with all elements
-- | being booleans.
-- |
-- | Result type for `MultiChoiceField` field is generated generically from
-- | provided sum type `opt` or from "Symbol list" which is implemented in
-- | Field.Option module.
-- |
-- | If you provide type like:
-- |
-- |  `data Choices = C1 | C2 | C3`
-- |
-- |  or
-- |
-- |  type Choices = "c1" :- "c2" :- Nil
-- |
-- | You can expect something like this as a result of validation:
-- |
-- |   { C1 ∷ Boolean
-- |   , C2 ∷ Boolean
-- |   , C3 ∷ Boolean
-- |   }
type MultiChoiceField e opt attrs =
  { name ∷ String
  , choices ∷ List (Tuple String opt)
  , value ∷ Either e (opt → Boolean)
  | attrs
  }

-- | This type class provides a way to transform simple sum type
-- | (constructors without args are only allowed) into: `FieldValidation`,
-- | `options` array which can be used in `Choice` record.
class Options opt where
  toOptionValueImpl ∷ opt → String

  optionsImpl ∷ (Proxy opt) → (List (Tuple String opt))
  -- | What about this Last wrapper here?
  -- | This `Last` wrapper is to simplify error handling
  -- | as it is has simple Semigroup instance which returns
  -- | last value from append chain.
  -- | I'm dropping `Last` finally in `toOptionValue` function.
  optionsParserImpl ∷ ∀ m. (Monad m) ⇒ (Proxy opt) → FieldValidation m (Last String) String opt

instance asOptionsSum ∷ (Options a, Options b) ⇒ Options (Sum a b) where
  toOptionValueImpl (Inl v) = toOptionValueImpl v
  toOptionValueImpl (Inr v) = toOptionValueImpl v

  optionsImpl _ =
    map (Inl <$> _) (optionsImpl (Proxy ∷ Proxy a))
    <> map (Inr <$> _) (optionsImpl (Proxy ∷ Proxy b))

  optionsParserImpl _ =
    (Inl <$> optionsParserImpl (Proxy ∷ Proxy a)) <|> (Inr <$> optionsParserImpl (Proxy ∷ Proxy b))

instance asOptionsConstructor ∷ (IsSymbol name) ⇒ Options (Constructor name NoArguments) where
  toOptionValueImpl _ = reflectSymbol (SProxy ∷ SProxy name)

  optionsImpl p =
    singleton (Tuple value option)
   where
    option = ((Constructor NoArguments) ∷ Constructor name NoArguments)
    value = reflectSymbol (SProxy ∷ SProxy name)

  optionsParserImpl _ =
    validate \s →
      if s == value
        then
          -- Right (Constructor name NoArguments) -
          Right ((Constructor NoArguments) ∷ Constructor name NoArguments)
        else
          Left (Last s)
   where
    value = reflectSymbol (SProxy ∷ SProxy name)

toOptionValue ∷ ∀ opt optRep. (Generic opt optRep) ⇒ (Options optRep) ⇒ opt → String
toOptionValue v = toOptionValueImpl (from v)

options ∷ ∀ a aRep. (Generic a aRep) ⇒ (Options aRep) ⇒ Proxy a → List (Tuple String a)
options _ = map (to <$> _) (optionsImpl (Proxy ∷ Proxy aRep))

optionsParser ∷ ∀ a aRep m. (Monad m) ⇒ (Generic a aRep) ⇒ (Options aRep) ⇒ Proxy a → FieldValidation m String String a
optionsParser _ = withException unwrap $ to <$> (optionsParserImpl (Proxy ∷ Proxy aRep))

class Choices c (c' ∷ # Type) | c → c' where
  choicesParserImpl ∷ ∀ m. (Monad m) ⇒ (Proxy c) → FieldValidation m String (Array String) { product ∷ (Record c'), checkOpt ∷ c → Boolean }

instance choicesConstructor ∷ (IsSymbol name, RowCons name Boolean () row, RowLacks name ()) ⇒ Choices (Constructor name NoArguments) row where
  choicesParserImpl proxy =
    parser
   where
    parser =
      let
        _name = (SProxy ∷ SProxy name)
        v = any (reflectSymbol _name == _)
        product i = insert _name  (v i) {}
        checkOpt i _ = v i
      in pureV $ \i → { product: product i, checkOpt: checkOpt i }

instance choicesSum ∷ (IsSymbol name, Choices b br, RowCons name Boolean br row, RowLacks name br) ⇒ Choices (Sum (Constructor name NoArguments) b) row where
  choicesParserImpl proxy =
    parser
   where
    parser = do
      i ← ask
      { product, checkOpt } ← choicesParserImpl (Proxy ∷ Proxy b)
      let
        _name = (SProxy ∷ SProxy name)
        v = any (reflectSymbol _name == _)
        checkOpt' = case _ of
          (Inl _) → v i
          (Inr b) → checkOpt b
      pure $ { product: insert _name  (v i) product, checkOpt: checkOpt' }

choicesParser ∷ ∀ a aRep row m
  . (Monad m)
  ⇒ (Generic a aRep)
  ⇒ (Choices aRep row)
  ⇒ Proxy a
  → FieldValidation m String (Array String) { product ∷ Record row, checkOpt ∷ a → Boolean }
choicesParser _ = do
  { product, checkOpt } ← choicesParserImpl (Proxy ∷ Proxy aRep)
  pure { product, checkOpt: checkOpt <<< from }

