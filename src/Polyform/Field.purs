module Polyform.Field where

-- import Control.Alt ((<|>))
-- import Control.Monad.Reader.Class (ask)
-- import Data.Either (Either(..))
-- import Data.Foldable (any)
-- import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), from, to)
-- import Data.List (List, singleton)
-- import Data.Newtype (class Newtype, unwrap)
-- import Data.Record (insert)
-- import Data.Tuple (Tuple(Tuple))
-- import Data.Validation.Polyform.Field (class Choices, class Options, choicesParserImpl, optionsImpl, optionsParserImpl, toOptionValueImpl)
-- import Data.Validation.Polyform.Validation.Field (FieldValidation, pureV, validate, withException)
-- import Prelude
-- import Type.Prelude (class IsSymbol, class RowLacks, Proxy(..), SProxy(..), reflectSymbol)

import Data.Either (Either)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Polyform.Validation as Validation
import Prelude


-- | Field validation just aggregates errors in list
type Validation m e a b = Validation.Validation m (List e) a b

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

type ChoiceField e opt attrs =
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

