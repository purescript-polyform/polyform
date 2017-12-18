module Data.Validation.Polyform.Field where

import Data.Either (Either)
import Data.List (List)
import Data.Tuple (Tuple)

-- | This module provides some helpers for building basic HTML fields.
-- | Fields represantation is as minimal as possible, so only
-- | validation relevant pieces are provided here.
-- | It also operates on really trivial form type
-- | `type Form field = List field`, because we are only
-- | interested here to provide building blocks which
-- | handle transition from `FieldValidation` to `Validation`.

-- | This module provides some helpers for building basic HTML fields.
-- | Fields represantation is as minimal as possible, so only
-- | validation relevant pieces are provided here.
-- | It also operates on really trivial form type
-- | `type Form field = List field`, because we are only
-- | interested here to provide building blocks which
-- | handle transition from `FieldValidation` to `Validation`.

type Form field = List field

-- | Don't look for single sum type or Variant which represents
-- | all these fields because it is not implemented here.

type Input e a =
  { name ∷ String
  , value ∷ Either e a
  }

-- XXX: why String?
type Select e opt =
  { name ∷ String
  , options ∷ List (Tuple String opt) -- Array { value ∷ String, option ∷ opt}
  , value ∷ Either e String
  }

-- | Multiple choices field resulting type is generically created from
-- | provided RowList or for convenience Sum type.
-- |
-- | If you provide type like:
-- |  `data Choices = C1 | C2 | C3`
-- |
-- | The resulting representation would be:
-- |
-- |   { c1 ∷ Boolean
-- |   , c2 ∷ Boolean
-- |   , c3 ∷ Boolean
-- |   }

class Choice choice c | choice → c

type Choices e choice =
  ∀ c. (Choice choice c) ⇒
  { name ∷ String
  , choices ∷ List (Tuple String c)
  , value ∷ Either e choice
  }
