module Polyform.Validator.Par where

import Prelude

import Control.Alt (class Alt)
import Control.Parallel (class Parallel)
import Control.Parallel as Parallel
import Control.Plus (class Plus, empty)
import Data.Functor.Compose (Compose(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor.Star (Star(..))
import Data.Validation.Semigroup (validation)
import Polyform.Validator (Validator(..))

newtype Par m e i o = Par (Validator m e i o)
derive instance newtypeVaildation ∷ Newtype (Par m e i o) _
derive instance functorPar ∷ (Applicative m, Semigroup e) ⇒ Functor (Par m e i)

instance applyPar ∷ (Monad m, Parallel f m, Semigroup e) ⇒ Apply (Par m e i) where
  apply (Par (Validator (Star mf))) (Par (Validator (Star ma))) =
    Par $ Validator $ Star $ \i → Compose $ Parallel.sequential ado
      f ← Parallel.parallel (unwrap $ mf i)
      a ← Parallel.parallel (unwrap $ ma i)
      in (f <*> a)

instance applicativePar ∷ (Monad m, Parallel f m, Monoid e) ⇒ Applicative (Par m e i) where
  pure = Par <<< pure

instance altPar ∷ (Monad m, Parallel f m, Monoid e) ⇒ Alt (Par m e i) where
  alt (Par (Validator (Star mv1))) (Par (Validator (Star mv2))) =
    Par $ Validator $ Star $ f
    where
      f i = Compose $ Parallel.sequential ado
        r1 ← Parallel.parallel (unwrap $ mv1 i)
        r2 ← Parallel.parallel (unwrap $ mv2 i)
        in (validation (const r2) pure r1)

instance plusPar ∷ (Monad m, Monoid e, Parallel f m) ⇒ Plus (Par m e i) where
  empty = Par empty

instance semigroupPar ∷ (Parallel f m, Semigroup e, Semigroup o) ⇒ Semigroup (Par m e i o) where
  append (Par (Validator (Star v1))) (Par (Validator (Star v2))) =
    Par $ Validator $ Star \i → Compose $ Parallel.sequential ado
      o1 ← Parallel.parallel (unwrap $ v1 i)
      o2 ← Parallel.parallel (unwrap $ v2 i)
      in o1 <> o2

instance monoidPar ∷ (Applicative m, Monoid e, Monoid o, Parallel f m) ⇒ Monoid (Par m e i o) where
  mempty = Par mempty

instance semigroupoidPar ∷ (Monad m, Semigroup e) ⇒ Semigroupoid (Par m e) where
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
