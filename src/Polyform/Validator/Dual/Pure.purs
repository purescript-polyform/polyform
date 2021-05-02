module Polyform.Validator.Dual.Pure where

import Prelude

import Data.Identity (Identity(..))
import Data.Newtype (un)
import Data.Validation.Semigroup (V)
import Polyform.Validator.Dual (Dual) as Validator.Dual
import Polyform.Validator.Dual (hoist, runSerializer, runValidator) as Dual

type Dual e i o = Validator.Dual.Dual Identity e i o

generalize ∷ ∀ e i m o. Applicative m ⇒ Dual e i o → Validator.Dual.Dual m e i o
generalize = Dual.hoist g
  where
    g ∷ ∀ a. Identity a → m a
    g (Identity a) = pure a

runValidator ∷ ∀ e i o. Dual e i o → i → V e o
runValidator dual = un Identity <<< Dual.runValidator dual

-- | TODO: Drop this.
runSerializer ∷ ∀ e i o. Dual e i o → o → i
runSerializer dual = Dual.runSerializer dual
