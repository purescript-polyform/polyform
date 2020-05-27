module Polyform.Reporter.R where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Plus (class Plus)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Validation.Semigroup (V, invalid, unV)

-- | This `R` can be seen more as a validation "report".
-- | It is isomoprhic to (Tuple r (Maybe a)) and probably
-- | could be migrated to this representation...
-- |
-- | Sometimes you want to collect the whole report of a validation
-- | instead of just validated value or error.
-- | For example when you are building a HTML form you want to
-- | display fields which were validated successfuly alongside with fields
-- | for which validation failed.
data R r a = Failure r | Success r a
derive instance eqR ∷ (Eq r, Eq a) ⇒ Eq (R r a)
derive instance ordR ∷ (Ord r, Ord a) ⇒ Ord (R r a)
derive instance eq1R ∷ (Eq r) ⇒ Eq1 (R r)

instance showR ∷ (Show r, Show a) ⇒ Show (R r a) where
  show (Failure r) = "(Failure " <> show r <> ")"
  show (Success r a) = "(Success " <> show r <> " " <> show a <> ")"

derive instance functorR ∷ Functor (R r)

instance bifunctorR ∷ Bifunctor R where
  bimap f _ (Failure r) = Failure (f r)
  bimap f g (Success r a) = Success (f r) (g a)

instance applyR ∷ (Semigroup r) ⇒ Apply (R r) where
  apply (Success r1 f) (Success r2 a) = Success (r1 <> r2) (f a)
  apply (Failure r1) (Success r2 _) = Failure (r1 <> r2)
  apply (Failure r1) (Failure r2) = Failure (r1 <> r2)
  apply (Success r1 _) (Failure r2) = Failure (r1 <> r2)

instance applicativeR ∷ (Monoid e) ⇒ Applicative (R e) where
  pure a = Success mempty a

-- | This instance uses first valid value and accumulates all reports.
-- |
-- | If you need "dual" strategy just use apply which "prefers" invalid results:
-- |
-- | pure (Success r1 a1) *> pure (Failure r2) = Failure (r1 <> r2)
-- | pure (Failure r1) *> pure (Success r2 a2) = Failure (r1 <> r2)
-- | pure (Success r1 a1) *> pure (Success r2 a2) = Success (r1 <> r2) a2
instance altR ∷ (Semigroup r) ⇒ Alt (R r) where
  alt (Success r1 a) (Success r2 _) = Success (r1 <> r2) a
  alt (Success r1 a) (Failure r2) = Success (r1 <> r2) a
  alt (Failure r1) (Success r2 a) = Success (r1 <> r2) a
  alt (Failure r1) (Failure r2) = Failure (r1 <> r2)

instance plusR ∷ (Monoid r) ⇒ Plus (R r) where
  empty = Failure mempty

instance alternativeR ∷ (Monoid r) ⇒ Alternative (R r)

-- | Default `Semigroup` instance appends valid and invalid
-- | parts of our `R`.
instance semigroupR ∷ (Semigroup r, Semigroup a) ⇒ Semigroup (R r a) where
  append = lift2 append

instance monoidR ∷ (Monoid r, Monoid a) ⇒ Monoid (R r a) where
  mempty = pure mempty

isSuccess ∷ ∀ a r. R r a → Boolean
isSuccess (Success _ _) = true
isSuccess _ = false

isFailure ∷ ∀ r a. R r a → Boolean
isFailure = not <<< isSuccess

report  ∷ ∀ r a. R r a → r
report (Success r _) = r
report (Failure r) = r

-- | Building `R` with possibly empty report for failure.
liftEither ∷ ∀ r. (Monoid r) ⇒ Either r ~> R r
liftEither (Left r) = Failure r
liftEither (Right a) = Success mempty a

liftEitherWith ∷ ∀ a r. (a → r) → Either r a → R r a
liftEitherWith _ (Left r) = Failure r
liftEitherWith f (Right a) = Success (f a) a

-- | Loosing report of failure value.
toEither ∷ ∀ r. R r ~> Either r
toEither (Failure r) = Left r
toEither (Success _ a) = Right a

-- | Building `R` with possibly empty report for failure.
liftVWith ∷ ∀ a e r. (e → r) → (a → r) → V e a → R r a
liftVWith f g = unV (Failure <<< f) (\a → Success (g a) a)

liftVWith' ∷ ∀ a r. (a → r) → V r a → R r a
liftVWith' f = liftVWith identity f

liftV ∷ ∀ r. Monoid r ⇒ V r ~> R r
liftV = liftVWith' (const $ mempty)

-- | Loosing report of failure value.
toV ∷ ∀ a r. Semigroup r ⇒ R r a → V r a
toV (Failure r) = invalid r
toV (Success _ a) = pure a

