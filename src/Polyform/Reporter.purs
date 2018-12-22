module Polyform.Reporter where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, empty)
import Control.Apply (lift2)
import Control.Plus (class Plus)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid, unV)
import Polyform.Validator (Validator(..))

-- | This `R` can be seen more as validation "reporter"
-- | then only failure / success carrier.
-- | Sometimes you want to collect the whole report of a validatio.
-- | instead of just validated value or error.
-- | For example when you are building a HTML form you want to
-- | display fields which were validated successfuly alongside with fields
-- | which validation failed.
data R r a = Failure r | Success r a
derive instance eqR ∷ (Eq r, Eq a) ⇒ Eq (R r a)
derive instance ordR ∷ (Ord r, Ord a) ⇒ Ord (R r a)
instance showR ∷ (Show r, Show a) ⇒ Show (R r a) where
  show (Failure r) = "(Failure " <> show r <> ")"
  show (Success r a) = "(Success " <> show r <> " " <> show a <> ")"

derive instance functorR ∷ Functor (R r)

instance bifunctorR ∷ Bifunctor R where
  bimap f _ (Failure r) = Failure (f r)
  bimap f g (Success r a) = Success (f r) (g a)

-- | All these `R` instances are quite borring as they just
-- | `append` underling report but it seems that they are really
-- | useful in practice.
instance applyR ∷ (Semigroup r) ⇒ Apply (R r) where
  apply (Success r1 f) (Success r2 a) = Success (r1 <> r2) (f a)
  apply (Failure r1) (Success r2 _) = Failure (r1 <> r2)
  apply (Failure r1) (Failure r2) = Failure (r1 <> r2)
  apply (Success r1 _) (Failure r2) = Failure (r1 <> r2)

instance applicativeR ∷ (Monoid e) ⇒ Applicative (R e) where
  pure a = Success mempty a

-- | This instance uses first valid value and accumulates all values.
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

isFailure ∷ ∀ a r. R a r → Boolean
isFailure = not <<< isSuccess

-- | Building `R` with possibly empty report for failure.
fromEither ∷ ∀ a r. (Monoid r) ⇒ Either r a → R r a
fromEither (Left r) = Failure r
fromEither (Right a) = Success mempty a

fromEitherWith ∷ ∀ a r. (a → r) → Either r a → R r a
fromEitherWith _ (Left r) = Failure r
fromEitherWith f (Right a) = Success (f a) a

-- | Loosing report of failure value.
toEither ∷ ∀ a r. R r a → Either r a
toEither (Failure r) = Left r
toEither (Success _ a) = Right a

-- | Building `R` with possibly empty report for failure.
fromVWith ∷ ∀ a e r. (e → r) → (a → r) → V e a → R r a
fromVWith f g = unV (Failure <<< f) (\a → Success (g a) a)

fromVWith' ∷ ∀ a r. (a → r) → V r a → R r a
fromVWith' f = fromVWith identity f

fromV ∷ ∀ a r. Monoid r ⇒ V r a → R r a
fromV = fromVWith' (const $ mempty)

-- | Loosing report of failure value.
toV ∷ ∀ a r. Semigroup r ⇒ R r a → V r a
toV (Failure r) = invalid r
toV (Success _ a) = pure a

newtype Reporter m r i o = Reporter (i → m (R r o))
derive instance newtypeRaildation ∷ Newtype (Reporter m r i b) _
derive instance functorReporter ∷ (Functor m) ⇒ Functor (Reporter m r i)

instance applyReporter ∷ (Semigroup r, Monad m) ⇒ Apply (Reporter m r i) where
  apply vf va = Reporter $ \i → do
    vf' ← unwrap vf i
    va' ← unwrap va i
    pure $ vf' <*> va'

instance applicativeReporter ∷ (Monoid r, Monad m) ⇒ Applicative (Reporter m r i) where
  pure = Reporter <<< const <<< pure <<< pure

instance altReporter ∷ (Monad m, Semigroup r) ⇒ Alt (Reporter m r i) where
  alt v1 v2 = Reporter \i → do
    v1' ← unwrap v1 i
    v2' ← unwrap v2 i
    pure $ v1' <|> v2'

instance plusReporter ∷ (Monad m, Monoid r) ⇒ Plus (Reporter m r i) where
  empty = Reporter <<< const <<< pure $ empty

instance semigroupReporter ∷ (Apply m, Semigroup r, Semigroup o) ⇒ Semigroup (Reporter m r i o) where
  append (Reporter r1) (Reporter r2) = Reporter (\i → (<>) <$> r1 i <*> r2 i)

instance monoidReporter ∷ (Applicative m, Monoid r, Monoid o) ⇒ Monoid (Reporter m r i o) where
  mempty = Reporter <<< const <<< pure $ mempty

instance semigroupoidReporter ∷ (Monad m, Semigroup r) ⇒ Semigroupoid (Reporter m r) where
  compose v2 v1 =
    Reporter $ (\i → do
      eb ← unwrap v1 i
      case eb of
        Success r o → do
          res ← unwrap v2 o
          pure $ case res of
            Success r' c → Success (r <> r') c
            Failure r' → Failure (r <> r')
        Failure r → pure (Failure r))

instance categoryReporter ∷ (Monad m, Monoid r) ⇒ Category (Reporter m r) where
  identity = Reporter $ pure <<< pure

instance profunctorReporter ∷ (Monad m, Monoid r) ⇒ Profunctor (Reporter m r) where
  dimap l r v = (hoistFn l) >>> v >>> (hoistFn r)

instance choiceReporter ∷ (Monad m, Monoid r) ⇒ Choice (Reporter m r) where
  left v = Reporter (case _ of
    Left i → map Left <$> runReporter v i
    Right r → pure (Success mempty (Right r)))

  right v = Reporter (case _ of
    Right i → map Right <$> runReporter v i
    Left l → pure (Success mempty (Left l)))

instance strongReporter ∷ (Monad m, Monoid e) ⇒ Strong (Reporter m e) where
  first v = Reporter (\(Tuple f s) → map (\f' → Tuple f' s) <$> runReporter v f)
  second v = Reporter (\(Tuple f s) → map (\s' → Tuple f s') <$> runReporter v s)

runReporter ∷ ∀ i o m r. Reporter m r i o → (i → m (R r o))
runReporter = unwrap

ask ∷ ∀ i m r. Applicative m ⇒ Monoid r ⇒ Reporter m r i i
ask = hoistFn identity

hoistFn ∷ ∀ i m o r. Applicative m ⇒ Monoid r ⇒ (i → o) → Reporter m r i o
hoistFn f = Reporter $ f >>> pure >>> pure

hoistFnR ∷ ∀ i m o r. Applicative m ⇒ (i → R r o) → Reporter m r i o
hoistFnR f = Reporter $ f >>> pure

hoistFnMR ∷ ∀ i m o r. (i → m (R r o)) → Reporter m r i o
hoistFnMR f = Reporter f

hoistFnEither ∷ ∀ e i m o. Applicative m ⇒ Monoid e ⇒ (i → Either e o) → Reporter m e i o
hoistFnEither f = hoistFnR $ f >>> fromEither

hoistFnEitherWith ∷ ∀ e i m o. Applicative m ⇒ (o → e) → (i → Either e o) → Reporter m e i o
hoistFnEitherWith f g = hoistFnR $ g >>> fromEitherWith f

hoistToValidator ∷ ∀ e i m. Functor m ⇒ Monoid e ⇒ Reporter m e i ~> Validator m e i
hoistToValidator (Reporter f) = Validator (f >>> map toV)

-- | Building `Reporter` from `Validator` with possibly empty failure report.
hoistValidator ∷ ∀ e i m. Functor m ⇒ Monoid e ⇒ Validator m e i ~> Reporter m e i
hoistValidator (Validator r) = Reporter (r >>> map fromV)

-- | Building `Reporter` from `Validator` by creating report from error and from value.
hoistValidatorWith ∷ ∀ e i m o r. Functor m ⇒ (e → r) → (o → r) → Validator m e i o → Reporter m r i o
hoistValidatorWith f g (Validator r) = Reporter (r >>> map (fromVWith f g))

-- | Building `Reporter` from `Validator` by creating report from value.
hoistValidatorWith' ∷ ∀ e i m o. Functor m ⇒ (o → e) → Validator m e i o → Reporter m e i o
hoistValidatorWith' f (Validator r) = Reporter (r >>> map (fromVWith' f))

-- | Provides access to validation result so you can
-- | `bimap` over `r` and `b` type in resulting `R r b`.
newtype BifunctorReporter m i r o = BifunctorReporter (Reporter m r i o)
derive instance newtypeBifunctorReporter ∷ Newtype (BifunctorReporter m i r o) _

instance bifunctorBifunctorReporter ∷ Monad m ⇒ Bifunctor (BifunctorReporter m i) where
  bimap l r (BifunctorReporter (Reporter f)) = BifunctorReporter $ Reporter $ \i → do
    v ← f i
    pure $ bimap l r v

bimapReporter ∷ ∀ i m o o' r r'
  . (Monad m)
  ⇒ (r → r')
  → (o → o')
  → Reporter m r i o
  → Reporter m r' i o'
bimapReporter l r = unwrap <<< bimap l r <<< BifunctorReporter

lmapReporter ∷ ∀ i m o r r'. Monad m ⇒ (r → r') → Reporter m r i o → Reporter m r' i o
lmapReporter l = unwrap <<< lmap l <<< BifunctorReporter

rmapReporter ∷ ∀ i m o o' r. Monad m ⇒ (o → o') → Reporter m r i o → Reporter m r i o'
rmapReporter l = unwrap <<< rmap l <<< BifunctorReporter
