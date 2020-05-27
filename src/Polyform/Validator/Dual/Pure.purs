module Polyform.Validator.Dual.Pure where

import Prelude

import Data.Identity (Identity(..))
import Polyform.Validator.Dual (Dual) as Validator.Dual
import Polyform.Validator.Dual (hoist) as Dual

type Dual e i o = Validator.Dual.Dual Identity Identity e i o

generalize ∷ ∀ e i m o s. Applicative m ⇒ Applicative s ⇒ Dual e i o → Validator.Dual.Dual m s e i o
generalize = Dual.hoist pg sg
  where
    pg ∷ ∀ a. Identity a → m a
    pg (Identity a) = pure a

    sg ∷ ∀ a. Identity a → s a
    sg (Identity a) = pure a

