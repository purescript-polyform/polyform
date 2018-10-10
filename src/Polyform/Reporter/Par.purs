module Polyform.Reporter.Par where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Parallel (class Parallel)
import Control.Parallel as Parallel
import Data.Newtype (class Newtype)
import Polyform.Reporter (Reporter(..))

newtype ParReporter m r a b = ParReporter (Reporter m r a b)
derive instance newtypeVaildation ∷ Newtype (ParReporter m r a b) _
derive instance functorParReporter ∷ (Functor m) ⇒ Functor (ParReporter m r a)

instance applyParReporter ∷ (Monad m, Parallel f m, Semigroup r) ⇒ Apply (ParReporter m r a) where
  apply (ParReporter (Reporter mf)) (ParReporter (Reporter ma)) =
    ParReporter $ Reporter \i →
      Parallel.sequential $ (<*>) <$> Parallel.parallel (mf i) <*> Parallel.parallel (ma i)

instance applicativeParReporter ∷ (Monad m, Parallel f m, Monoid r) ⇒ Applicative (ParReporter m r a) where
  pure = ParReporter <<< pure

instance altParReporter ∷ (Monad m, Parallel f m, Monoid r) ⇒ Alt (ParReporter m r a) where
  alt (ParReporter (Reporter mv1)) (ParReporter (Reporter mv2)) =
    ParReporter $ Reporter \i →
      Parallel.sequential $ ((<|>) <$> Parallel.parallel (mv1 i) <*> Parallel.parallel (mv2 i))

-- | As we are not able to provide `Parallel` instance currently
-- | (https://github.com/purescript/purescript-parallel/issues/24)
-- | here we have some synonims.
parallel
  ∷ ∀ a b r m
  . Reporter m r a b
  → ParReporter m r a b
parallel = ParReporter

sequential
  ∷ ∀ a b r m
  . ParReporter m r a b
  → Reporter m r a b
sequential (ParReporter v) = v

