module Polyform.Reporter where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, empty)
import Control.Apply (lift2)
import Control.Plus (class Plus)
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Functor.Compose (Compose(..), bihoistCompose)
import Data.Newtype (class Newtype, un, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Star (Star(..), hoistStar)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..), invalid, unV)
import Polyform.Validator (Validator(..))

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

newtype Reporter m r i o = Reporter (Star (Compose m (R r)) i o)
derive instance newtypeReporter ∷ Newtype (Reporter m r i b) _
derive newtype instance functorReporter ∷ (Functor m) ⇒ Functor (Reporter m e i)
derive newtype instance applyReporter ∷ (Applicative m, Semigroup e) ⇒ Apply (Reporter m e i)
derive newtype instance applicativeReporter ∷ (Applicative m, Monoid e) ⇒ Applicative (Reporter m e i)
derive newtype instance profunctorReporter ∷ Functor m ⇒ Profunctor (Reporter m e)
derive newtype instance choiceReporter ∷ (Monoid e, Applicative m) ⇒ Choice (Reporter m e)
derive newtype instance strongReporter ∷ (Monad m, Semigroup e) ⇒ Strong (Reporter m e)

instance altReporter ∷ (Monad m, Semigroup r) ⇒ Alt (Reporter m r i) where
  alt v1 v2 = Reporter $ Star $ \i → Compose do
    v1' ← runReporter v1 i
    v2' ← runReporter v2 i
    pure $ v1' <|> v2'

instance plusReporter ∷ (Monad m, Monoid r) ⇒ Plus (Reporter m r i) where
  empty = Reporter <<< Star <<< const <<< Compose <<< pure $ empty

instance semigroupReporter ∷ (Apply m, Semigroup r, Semigroup o) ⇒ Semigroup (Reporter m r i o) where
  append (Reporter (Star r1)) (Reporter (Star r2)) = Reporter (Star (\i → (<>) <$> r1 i <*> r2 i))

instance monoidReporter ∷ (Applicative m, Monoid r, Monoid o) ⇒ Monoid (Reporter m r i o) where
  mempty = Reporter <<< Star <<< const <<< Compose <<< pure $ mempty

instance semigroupoidReporter ∷ (Monad m, Semigroup r) ⇒ Semigroupoid (Reporter m r) where
  compose v2 v1 =
    Reporter $ Star (\i → Compose $ do
      eb ← runReporter v1 i
      case eb of
        Success r o → do
          res ← runReporter v2 o
          pure $ case res of
            Success r' c → Success (r <> r') c
            Failure r' → Failure (r <> r')
        Failure r → (pure (Failure r)))

instance categoryReporter ∷ (Monad m, Monoid r) ⇒ Category (Reporter m r) where
  identity = Reporter (Star (Compose <<< pure <<< pure))

runReporter ∷ ∀ i o m r. Reporter m r i o → (i → m (R r o))
runReporter (Reporter (Star f)) = f >>> (un Compose)

ask ∷ ∀ i m r. Applicative m ⇒ Monoid r ⇒ Reporter m r i i
ask = hoistFn identity

hoist ∷ ∀ e i m m'. Functor m ⇒ (m ~> m') → Reporter m e i ~> Reporter m' e i
hoist nt (Reporter r) = Reporter (hoistStar (bihoistCompose nt identity) r)

-- | Building `Reporter` from `Validator` with possibly empty failure report.
hoistValidator ∷ ∀ e i m. Functor m ⇒ Monoid e ⇒ Validator m e i ~> Reporter m e i
hoistValidator (Validator (Star r)) = Reporter (Star (r >>> un Compose >>> map fromV >>> Compose))

hoistToValidator ∷ ∀ e i m. Functor m ⇒ Monoid e ⇒ Reporter m e i ~> Validator m e i
hoistToValidator (Reporter (Star f)) = Validator (Star (f >>> un Compose >>> map toV >>> Compose))


hoistFn ∷ ∀ e i m o. Applicative m ⇒ Monoid e ⇒ (i → o) → Reporter m e i o
hoistFn f = Reporter $ Star $ f >>> pure >>> pure >>> Compose

hoistFnR ∷ ∀ e i m o. Applicative m ⇒ Semigroup e ⇒ (i → R e o) → Reporter m e i o
hoistFnR f = Reporter $ Star $ f >>> pure >>> Compose

hoistFnMR ∷ ∀ e i m o. (i → m (R e o)) → Reporter m e i o
hoistFnMR f = Reporter $ Star $ map Compose f

hoistFnEither ∷ ∀ e i m o. Applicative m ⇒ Monoid e ⇒ (i → Either e o) → Reporter m e i o
hoistFnEither f = hoistFnR $ f >>> fromEither


-- | Builders which look under the hood
fromFnEitherWith ∷ ∀ e i m o. Semigroup e ⇒ Applicative m ⇒ (o → e) → (i → Either e o) → Reporter m e i o
fromFnEitherWith f g = hoistFnR $ g >>> fromEitherWith f

-- | Building `Reporter` from `Validator` by creating report from error and from value using also an input.
fromValidatorWith ∷ ∀ e i m o r. Functor m ⇒ (Tuple i e → r) → (Tuple i o → r) → Validator m e i o → Reporter m r i o
fromValidatorWith f g (Validator (Star r)) = Reporter $ Star $ \i →
  (r >>> un Compose >>> map (fromVWith (f <<< Tuple i) (g <<< Tuple i)) >>> Compose $ i)

-- | Building `Reporter` from `Validator` by creating report from error but leaving error/report type untouched.
fromValidatorWith' ∷ ∀ e i m o. Functor m ⇒ (Tuple i o → e) → Validator m e i o → Reporter m e i o
fromValidatorWith' f (Validator (Star r)) = Reporter $ Star $ \i →
  (r >>> un Compose >>> map (fromVWith' (f <<< Tuple i)) >>> Compose $ i)

fromValidatorWithM ∷ ∀ e i m o r. Monad m ⇒ (Tuple i e → m r) → (Tuple i o → m r) → Validator m e i o → Reporter m r i o
fromValidatorWithM f g (Validator (Star s)) = Reporter $ Star $ \i → Compose do
  r ← un Compose (s i)
  case r of
    V (Right o) → do
      e' ← g (Tuple i o)
      pure (Success e' o)
    V (Left e) → do
      e' ← f (Tuple i e)
      pure (Failure e')

-- | Provides access to validation result so you can
-- | `bimap` over `r` and `b` type in resulting `R r b`.
newtype BifunctorReporter m i r o = BifunctorReporter (Reporter m r i o)
derive instance newtypeBifunctorReporter ∷ Newtype (BifunctorReporter m i r o) _

instance bifunctorBifunctorReporter ∷ Monad m ⇒ Bifunctor (BifunctorReporter m i) where
  bimap l r (BifunctorReporter rep) = BifunctorReporter $ Reporter $ Star $ \i → Compose do
    v ← runReporter rep i
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

rmapReporter ∷ ∀ i m o o' r. Functor m ⇒ (o → o') → Reporter m r i o → Reporter m r i o'
rmapReporter = map
