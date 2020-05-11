module Polyform.Duals.Validator.Pure where

import Prelude

import Data.Identity (Identity(..))
import Polyform.Duals.Validator (Dual, hoist) as Duals.Validator

type Dual e i o = Duals.Validator.Dual Identity Identity e i o

generalize ∷ ∀ e i m o s. Applicative m ⇒ Applicative s ⇒ Duals.Validator.Dual Identity Identity e i o → Duals.Validator.Dual m s e i o
generalize = Duals.Validator.hoist pg sg
  where
    pg ∷ ∀ a. Identity a → m a
    pg (Identity a) = pure a

    sg ∷ ∀ a. Identity a → s a
    sg (Identity a) = pure a

