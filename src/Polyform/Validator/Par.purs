module Polyform.Validator.Par where

import Prelude

import Control.Alt (class Alt)
import Control.Parallel (class Parallel)
import Control.Parallel as Parallel
import Data.Newtype (class Newtype)
import Data.Validation.Semigroup (unV)
import Polyform.Validator (Validator(..))

newtype Par m r a b = Par (Validator m r a b)
derive instance newtypeVaildation ∷ Newtype (Par m r a b) _
derive instance functorPar ∷ (Functor m) ⇒ Functor (Par m r a)

instance applyPar ∷ (Monad m, Parallel f m, Semigroup r) ⇒ Apply (Par m r a) where
  apply (Par (Validator mf)) (Par (Validator ma)) =
    Par $ Validator \i →
      Parallel.sequential $ (<*>) <$> Parallel.parallel (mf i) <*> Parallel.parallel (ma i)

instance applicativePar ∷ (Monad m, Parallel f m, Monoid r) ⇒ Applicative (Par m r a) where
  pure = Par <<< pure

instance altPar ∷ (Monad m, Parallel f m, Monoid r) ⇒ Alt (Par m r a) where
  alt (Par (Validator mv1)) (Par (Validator mv2)) =
    Par $ Validator $ f
    where
      f i = Parallel.sequential $ ado
        r1 <- Parallel.parallel (mv1 i)
        r2 <- Parallel.parallel (mv2 i)
        in unV (const r2) pure r1

-- | As we are not able to provide `Parallel` instance currently
-- | (https://github.com/purescript/purescript-parallel/issues/24)
-- | here we have some synonims.
parallel
  ∷ ∀ a b r m
  . Validator m r a b
  → Par m r a b
parallel = Par

sequential
  ∷ ∀ a b r m
  . Par m r a b
  → Validator m r a b
sequential (Par v) = v

