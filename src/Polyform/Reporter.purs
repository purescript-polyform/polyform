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

-- | This instance uses first valid value:
-- |
-- | pure (Success r1 a) <|> pure (Failure r2) = Success r1 a
-- | pure (Success r1 a1) <|> pure (Success r2 a2) = Success r1 a1
-- | pure (Failure r1) <|> pure (Failure r2) = Failure (r1 <> r2)
-- |
-- | If you need "dual" strategy just use apply which "prefers" invalid results:
-- |
-- | pure (Success r1 a1) *> pure (Failure r2) = Failure (r1 <> r2)
-- | pure (Failure r1) *> pure (Success r2 a2) = Failure (r1 <> r2)
-- | pure (Success r1 a1) *> pure (Success r2 a2) = Success (r1 <> r2) a2
-- |
-- | If you find any other instance useful please provide an example,
-- | add a newtype wrapper and provide a PR with related tests.
instance altR ∷ (Semigroup r) ⇒ Alt (R r) where
  alt (Success r1 a) _ = Success r1 a
  alt _ (Success r2 a) = Success r2 a
  alt (Failure r1) (Failure r2) = Failure (r1 <> r2)

instance plusR ∷ (Monoid r) ⇒ Plus (R r) where
  empty = Failure mempty

instance alternativeR ∷ (Monoid r) ⇒ Alternative (R r)

-- | Defaul `Semigroup` instance appends valid and invalid
-- | parts of our `R`.
instance semigroupR ∷ (Semigroup r, Semigroup a) ⇒ Semigroup (R r a) where
  append = lift2 append

instance monoidR ∷ (Monoid r, Monoid a) ⇒ Monoid (R r a) where
  mempty = pure mempty

isSuccess ∷ ∀ a r. R r a → Boolean
isSuccess (Success _ _) = true
isSuccess _ = false

fromEither ∷ ∀ a r. (Monoid r) ⇒ Either r a → R r a
fromEither (Left r) = Failure r
fromEither (Right a) = Success mempty a

toEither ∷ ∀ a r. R r a → Either r a
toEither (Failure r) = Left r
toEither (Success _ a) = Right a

newtype Reporter m r a b = Reporter (a → m (R r b))
derive instance newtypeRaildation ∷ Newtype (Reporter m r a b) _
derive instance functorReporter ∷ (Functor m) ⇒ Functor (Reporter m r a)

instance applyReporter ∷ (Semigroup r, Monad m) ⇒ Apply (Reporter m r a) where
  apply vf va = Reporter $ \i → do
    vf' ← unwrap vf i
    va' ← unwrap va i
    pure $ vf' <*> va'

instance applicativeReporter ∷ (Monoid r, Monad m) ⇒ Applicative (Reporter m r a) where
  pure = Reporter <<< const <<< pure <<< pure

instance altReporter ∷ (Monoid r, Monad m) ⇒ Alt (Reporter m r a) where
  alt v1 v2 = Reporter \a → do
    v1' ← unwrap v1 a
    v2' ← unwrap v2 a
    pure $ v1' <|> v2'

instance plusReporter ∷ (Monad m, Monoid r) ⇒ Plus (Reporter m r a) where
  empty = Reporter <<< const <<< pure $ empty

instance semigroupReporter ∷ (Semigroup (m (R r b))) ⇒ Semigroup (Reporter m r a b) where
  append (Reporter v1) (Reporter v2) = Reporter (\a → v1 a <> v2 a)

instance monoidReporter ∷ (Applicative m, Monoid r, Monoid b, Semigroup (m (R r b))) ⇒ Monoid (Reporter m r a b) where
  mempty = Reporter <<< const <<< pure $ mempty

instance semigroupoidReporter ∷ (Monad m, Semigroup r) ⇒ Semigroupoid (Reporter m r) where
  compose v2 v1 =
    Reporter $ (\a → do
      eb ← unwrap v1 a
      case eb of
        Success r b → do
          res ← unwrap v2 b
          pure $ case res of
            Success r' c → Success (r <> r') c
            Failure r' → Failure (r <> r')
        Failure r → pure (Failure r))

instance categoryReporter ∷ (Monad m, Monoid r) ⇒ Category (Reporter m r) where
  identity = Reporter $ pure <<< pure

instance profunctorReporter ∷ (Monad m, Monoid r) ⇒ Profunctor (Reporter m r) where
  dimap l r v = (hoistFn l) >>> v >>> (hoistFn r)

-- XXX: Provide Strong instance too
instance choiceReporter ∷ (Monad m, Monoid r) ⇒ Choice (Reporter m r) where
  left v = Reporter (case _ of
    Left i → map Left <$> runReporter v i
    Right r → pure (Success mempty (Right r)))

  right v = Reporter (case _ of
    Right i → map Right <$> runReporter v i
    Left l → pure (Success mempty (Left l)))

runReporter ∷ ∀ a b r m. Reporter m r a b → (a → m (R r b))
runReporter = unwrap

ask ∷ ∀ a r m. Monad m ⇒ Monoid r ⇒ Reporter m r a a
ask = Reporter (\a → pure (Success mempty a))

hoistFn ∷ ∀ a b m r. Monad m ⇒ Monoid r ⇒ (a → b) → Reporter m r a b
hoistFn f = Reporter $ f >>> pure >>> pure

hoistFnR ∷ ∀ a b m r. Monad m ⇒ Monoid r ⇒ (a → R r b) → Reporter m r a b
hoistFnR f = Reporter $ f >>> pure

hoistFnMR ∷ ∀ a b m r. Monad m ⇒ Monoid r ⇒ (a → m (R r b)) → Reporter m r a b
hoistFnMR f = Reporter f

-- | Provides access to validation result
-- | so you can `bimap` over `r` and `b` type in resulting `R r b`.
newtype BifunctorReporter m a r b = BifunctorReporter (Reporter m r a b)
derive instance newtypeBifunctorReporter ∷ Newtype (BifunctorReporter m a r b) _

instance bifunctorBifunctorReporter ∷ Monad m ⇒ Bifunctor (BifunctorReporter m a) where
  bimap l r (BifunctorReporter (Reporter f)) = BifunctorReporter $ Reporter $ \a → do
    v ← f a
    pure $ bimap l r v

bimapReporter ∷ ∀ a b b' r r' m
  . (Monad m)
  ⇒ (r → r')
  → (b → b')
  → Reporter m r a b
  → Reporter m r' a b'
bimapReporter l r = unwrap <<< bimap l r <<< BifunctorReporter

lmapReporter ∷ ∀ a b m r r'. Monad m ⇒ (r → r') → Reporter m r a b → Reporter m r' a b
lmapReporter l = unwrap <<< lmap l <<< BifunctorReporter

rmapReporter ∷ ∀ a b b' m r. Monad m ⇒ (b → b') → Reporter m r a b → Reporter m r a b'
rmapReporter l = unwrap <<< rmap l <<< BifunctorReporter
