module Polyform.Validator.Par where

import Prelude

-- import Control.Alt (class Alt, (<|>))
import Control.Parallel (class Parallel)
import Control.Parallel as Parallel
import Data.Newtype (class Newtype)
import Polyform.Validator (Validator(..))

newtype ParValidator m r a b = ParValidator (Validator m r a b)
derive instance newtypeVaildation ∷ Newtype (ParValidator m r a b) _
derive instance functorParValidator ∷ (Functor m) ⇒ Functor (ParValidator m r a)

instance applyParValidator ∷ (Monad m, Parallel f m, Semigroup r) ⇒ Apply (ParValidator m r a) where
  apply (ParValidator (Validator mf)) (ParValidator (Validator ma)) =
    ParValidator $ Validator \i →
      Parallel.sequential $ (<*>) <$> Parallel.parallel (mf i) <*> Parallel.parallel (ma i)

instance applicativeParValidator ∷ (Monad m, Parallel f m, Monoid r) ⇒ Applicative (ParValidator m r a) where
  pure = ParValidator <<< pure

-- instance altParValidator ∷ (Monad m, Parallel f m, Monoid r) ⇒ Alt (ParValidator m r a) where
--   alt (ParValidator (Validator mv1)) (ParValidator (Validator mv2)) =
--     ParValidator $ Validator \i →
--       Parallel.sequential $ ((<|>) <$> Parallel.parallel (mv1 i) <*> Parallel.parallel (mv2 i))

-- | As we are not able to provide `Parallel` instance currently
-- | (https://github.com/purescript/purescript-parallel/issues/24)
-- | here we have some synonims.
parallel
  ∷ ∀ a b r m
  . Validator m r a b
  → ParValidator m r a b
parallel = ParValidator

sequential
  ∷ ∀ a b r m
  . ParValidator m r a b
  → Validator m r a b
sequential (ParValidator v) = v

