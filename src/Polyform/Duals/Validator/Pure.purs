module Polyform.Duals.Validator.Pure where

import Prelude

import Data.Identity (Identity(..))
import Polyform.Duals.Validator (Dual, hoist) as Duals.Validator

type Dual e i o = Duals.Validator.Dual Identity e i o

generalize ∷ ∀ e i m o. Applicative m ⇒ Duals.Validator.Dual Identity e i o → Duals.Validator.Dual m e i o
generalize = Duals.Validator.hoist g
  where
    g ∷ ∀ a. Identity a → m a
    g (Identity a) = pure a

