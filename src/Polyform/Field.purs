module Polyform.Field
  ( Input
  , SingleChoice
  , MultiChoice
  ) where

import Data.List (List)
import Data.Tuple (Tuple)
import Polyform.Validation (V)


-- | This module provides some very simple representations of HTML fields.
-- | Don't look for single sum type or Variant which ties all of them.
-- |
-- | Fields represantation is as minimal as possible, so only
-- | validation relevant pieces are exposed as attrs.

type Input attrs name e value =
  { name ∷ name
  , value ∷ V e value
  | attrs
  }

type SingleChoice e opt attrs name =
  { name ∷ name
  , choices ∷ List (Tuple String opt)
  , value ∷ V e opt
  | attrs
  }

-- | This field can be used to represent for example multiple checkboxes with the same
-- | `name` or `select multiple` - it's final value is record with all elements
-- | being Booleans.
-- |
-- | Result type for `MultiChoice` field can be statically and generically generated
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
-- | You can expect something like this as a result of validation
-- | (derived from `Field.Generic.multiChoiceParser`):
-- |
-- |   { C1 ∷ Boolean
-- |   , C2 ∷ Boolean
-- |   , C3 ∷ Boolean
-- |   }
-- |
-- |
-- | Of course you can provide your own representation for choices like list of `Strings` etc.
-- |
type MultiChoice e opt attrs name =
  { name ∷ name
  , choices ∷ List (Tuple String opt)
  , value ∷ V e (opt → Boolean)
  | attrs
  }

