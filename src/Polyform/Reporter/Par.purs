module Polyform.Reporter.Par where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Parallel (class Parallel)
import Control.Parallel as Parallel
import Data.Newtype (class Newtype)
import Polyform.Reporter (Reporter(..))

newtype Par m r a b = Par (Reporter m r a b)
derive instance newtypeVaildation ∷ Newtype (Par m r a b) _
derive instance functorPar ∷ (Functor m) ⇒ Functor (Par m r a)

instance applyPar ∷ (Monad m, Parallel f m, Semigroup r) ⇒ Apply (Par m r a) where
  apply (Par (Reporter mf)) (Par (Reporter ma)) =
    Par $ Reporter \i →
      Parallel.sequential $ (<*>) <$> Parallel.parallel (mf i) <*> Parallel.parallel (ma i)

instance applicativePar ∷ (Monad m, Parallel f m, Monoid r) ⇒ Applicative (Par m r a) where
  pure = Par <<< pure

instance altPar ∷ (Monad m, Parallel f m, Monoid r) ⇒ Alt (Par m r a) where
  alt (Par (Reporter mv1)) (Par (Reporter mv2)) =
    Par $ Reporter \i →
      Parallel.sequential $ ((<|>) <$> Parallel.parallel (mv1 i) <*> Parallel.parallel (mv2 i))

-- | As we are not able to provide `Parallel` instance currently
-- | (https://github.com/purescript/purescript-parallel/issues/24)
-- | here we have some synonims.
parallel
  ∷ ∀ a b r m
  . Reporter m r a b
  → Par m r a b
parallel = Par

sequential
  ∷ ∀ a b r m
  . Par m r a b
  → Reporter m r a b
sequential (Par v) = v

