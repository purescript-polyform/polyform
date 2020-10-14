module Polyform.Validator.Pure where

import Prelude

import Data.Identity (Identity(..))
import Data.Newtype (un)
import Data.Validation.Semigroup (V)
import Polyform.Validator (Validator, hoist, runValidator) as Validator

type Validator e i o = Validator.Validator Identity e i o

generalize ∷ ∀ e i m o. Applicative m ⇒ Validator e i o → Validator.Validator m e i o
generalize = Validator.hoist (case _ of Identity o → pure o)

runValidator ∷ ∀ e i o. Validator e i o → i → V e o
runValidator = map (un Identity) <<< Validator.runValidator
