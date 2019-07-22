module Polyform.Validator.Pure where

import Prelude

import Data.Identity (Identity(..))
import Polyform.Validator (Validator(..)) as Validator

type Validator e i o = Validator.Validator Identity e i o

generalize ∷ ∀ e i m o. Applicative m ⇒ Validator e i o → Validator.Validator m e i o
generalize (Validator.Validator f) = Validator.Validator \i → let Identity o = f i in pure o
