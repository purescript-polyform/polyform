module Polyform.Validator.Par where

import Prelude

import Control.Alt (class Alt)
import Control.Parallel (class Parallel)
import Control.Parallel as Parallel
import Control.Plus (class Plus, empty)
import Data.Newtype (class Newtype)
import Data.Profunctor.Star (Star(..))
import Data.Validation.Semigroup (unV)
import Polyform.Validator (Validator(..))

newtype Par m e i o = Par (Validator m e i o)
derive instance newtypeVaildation ∷ Newtype (Par m e i o) _
derive instance functorPar ∷ (Functor m) ⇒ Functor (Par m e i)

instance applyPar ∷ (Monad m, Parallel f m, Semigroup e) ⇒ Apply (Par m e i) where
  apply (Par (Validator (Star mf))) (Par (Validator (Star ma))) =
    Par $ Validator $ Star \i →
      Parallel.sequential $ (<*>) <$> Parallel.parallel (mf i) <*> Parallel.parallel (ma i)

instance applicativePar ∷ (Monad m, Parallel f m, Monoid e) ⇒ Applicative (Par m e i) where
  pure = Par <<< pure

instance altPar ∷ (Monad m, Parallel f m, Monoid e) ⇒ Alt (Par m e i) where
  alt (Par (Validator mv1)) (Par (Validator mv2)) =
    Par $ Validator $ f
    where
      f i = Parallel.sequential $ ado
        r1 <- Parallel.parallel (mv1 i)
        r2 <- Parallel.parallel (mv2 i)
        in unV (const r2) pure r1

instance plusPar ∷ (Monad m, Monoid e, Parallel f m) ⇒ Plus (Par m e i) where
  empty = Par empty

instance semigroupPar ∷ (Parallel f m, Semigroup e, Semigroup o) ⇒ Semigroup (Par m e i o) where
  append (Par (Validator (Star v1))) (Par (Validator (Star v2))) =
    Par $ Validator (\i → Parallel.sequential $ (<>) <$> Parallel.parallel (v1 i) <*> Parallel.parallel (v2 i))

instance monoidPar ∷ (Applicative m, Monoid e, Monoid o, Parallel f m) ⇒ Monoid (Par m e i o) where
  mempty = Par <<< Validator <<< pure <<< pure $ mempty

instance semigroupoidValidator ∷ (Monad m, Semigroup e) ⇒ Semigroupoid (Par m e) where
  compose (Par v2) (Par v1) = Par $ compose v2 v1

instance categoryValidator ∷ (Monad m, Monoid e) ⇒ Category (Par m e) where
  identity = Par identity

-- | As we are not able to provide `Parallel` instance currently
-- | (https://github.com/purescript/purescript-parallel/issues/24)
-- | here we have some synonims.
parallel
  ∷ ∀ i o e m
  . Validator m e i o
  → Par m e i o
parallel = Par

sequential
  ∷ ∀ i o e m
  . Par m e i o
  → Validator m e i o
sequential (Par v) = v

