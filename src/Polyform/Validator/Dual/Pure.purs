module Polyform.Validator.Dual.Pure where

import Prelude

import Data.Identity (Identity(..))
import Polyform.Validator.Dual (Dual) as Validator.Dual
import Polyform.Validator.Dual (hoist) as Dual

type Dual e i o = Validator.Dual.Dual Identity e i o

generalize ∷ ∀ e i m o. Applicative m ⇒ Dual e i o → Validator.Dual.Dual m e i o
generalize = Dual.hoist g
  where
    g ∷ ∀ a. Identity a → m a
    g (Identity a) = pure a


