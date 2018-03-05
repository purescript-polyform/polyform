module Polyform.Form.Validation.Par where

import Prelude

import Control.Parallel (class Parallel, parallel, sequential)
import Data.Newtype (class Newtype)
import Polyform.Form.Validation (Validation(..))

newtype ParValidation m e a b = ParValidation (Validation m e a b)
derive instance newtypeVaildation ∷ Newtype (ParValidation m e a b) _
derive instance functorParValidation ∷ (Functor m) ⇒ Functor (ParValidation m e a)

instance applyParValidation ∷ (Monad m, Parallel f m, Semigroup e) ⇒ Apply (ParValidation m e a) where
  apply (ParValidation (Validation mf)) (ParValidation (Validation ma)) =
    ParValidation $ Validation \i →
      sequential $ (<*>) <$> parallel (mf i) <*> parallel (ma i)

