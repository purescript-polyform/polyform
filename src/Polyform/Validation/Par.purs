module Polyform.Validation.Par where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Parallel (class Parallel)
import Control.Parallel as Parallel
import Data.Newtype (class Newtype)
import Polyform.Validation (Validation(..))

newtype ParValidation m e a b = ParValidation (Validation m e a b)
derive instance newtypeVaildation ∷ Newtype (ParValidation m e a b) _
derive instance functorParValidation ∷ (Functor m) ⇒ Functor (ParValidation m e a)

instance applyParValidation ∷ (Monad m, Parallel f m, Semigroup e) ⇒ Apply (ParValidation m e a) where
  apply (ParValidation (Validation mf)) (ParValidation (Validation ma)) =
    ParValidation $ Validation \i →
      Parallel.sequential $ (<*>) <$> Parallel.parallel (mf i) <*> Parallel.parallel (ma i)

instance applicativeParValidation ∷ (Monad m, Parallel f m, Monoid e) ⇒ Applicative (ParValidation m e a) where
  pure = ParValidation <<< pure

instance altParValidation ∷ (Monad m, Parallel f m, Monoid e) ⇒ Alt (ParValidation m e a) where
  alt (ParValidation (Validation mv1)) (ParValidation (Validation mv2)) =
    ParValidation $ Validation \i →
      Parallel.sequential $ ((<|>) <$> Parallel.parallel (mv1 i) <*> Parallel.parallel (mv2 i))

-- | As we are not able to provide `Parallel` instance currently
-- | (https://github.com/purescript/purescript-parallel/issues/24)
-- | here we have some synonims.
parallel
  ∷ ∀ a b e m
  . Validation m e a b
  → ParValidation m e a b
parallel = ParValidation

sequential
  ∷ ∀ a b e m
  . ParValidation m e a b
  → Validation m e a b
sequential (ParValidation v) = v

