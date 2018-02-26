module Polyform.Field
  ( InputField
  , ChoiceField
  , MultiChoiceField
  , module Validation
  ) where

import Data.Either (Either)
import Data.List (List)
import Data.Tuple (Tuple)
import Polyform.Field.Validation as Validation


-- | This module provides some very simple representations of HTML fields.
-- | Don't look for single sum type or Variant which ties all of them.
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

-- | This field can be used to represent for example multiple checkboxes with the same
-- | `name` or `select multiple` - it's final value is record with all elements
-- | being Booleans.
-- |
-- | Result type for `MultiChoiceField` field can be statically and generically generated
-- | by helpers from `Field.Generic`, so you can use sum type or "Symbol list" which
-- | (`Variant` stripped down to just a label) is implemented in `Field.Generic.Option` module.
-- |
-- | If you provide type like:
-- |
-- |  `data Choices = C1 | C2 | C3`
-- |
-- |  or
-- |
-- |  type Choices = "C1" :- "C2" :- Nil
-- |
-- | You can expect something like this as a result of validation:
-- |
-- |   { C1 ∷ Boolean
-- |   , C2 ∷ Boolean
-- |   , C3 ∷ Boolean
-- |   }
-- |
-- |
-- | Of course you can provide your own representation for choices like simple list of `Strings`
-- | which is generated at runtime etc.
-- |
type MultiChoiceField e opt attrs =
  { name ∷ String
  , choices ∷ List (Tuple String opt)
  , value ∷ Either e (opt → Boolean)
  | attrs
  }

