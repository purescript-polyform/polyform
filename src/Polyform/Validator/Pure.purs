module Polyform.Validator.Pure where

import Prelude

import Data.Identity (Identity(..))
import Polyform.Validator (Validator, hoist) as Validator

type Validator e i o = Validator.Validator Identity e i o

generalize ∷ ∀ e i m o. Applicative m ⇒ Validator e i o → Validator.Validator m e i o
generalize = Validator.hoist (case _ of Identity o → pure o)
