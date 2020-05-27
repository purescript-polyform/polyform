module Polyform.Reporter
  ( Reporter(..)
  , ask
  , bimapReporter
  , hoist
  , liftFn
  , liftFnEither
  , liftFnEitherWith
  , liftFnMR
  , liftFnR
  , liftValidator
  , liftValidatorWith
  , liftValidatorWith'
  , liftValidatorWithM
  , lmapReporter
  , module R
  , rmapReporter
  , runReporter
  , toValidator
  )
  where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (empty)
import Control.Plus (class Plus)
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Either (Either(..))
import Data.Functor.Compose (Compose(..), bihoistCompose)
import Data.Newtype (class Newtype, un, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Star (Star(..), hoistStar)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..))
import Polyform.Reporter.R (R(..))
import Polyform.Reporter.R (liftEither, liftEitherWith, liftV, liftVWith, liftVWith', toV, R(..)) as R
import Polyform.Validator (Validator(..))

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
ask = liftFn identity

hoist ∷ ∀ e i m m'. Functor m ⇒ (m ~> m') → Reporter m e i ~> Reporter m' e i
hoist nt (Reporter r) = Reporter (hoistStar (bihoistCompose nt identity) r)

-- | Building `Reporter` from `Validator` with possibly empty failure report.
liftValidator ∷ ∀ e i m. Functor m ⇒ Monoid e ⇒ Validator m e i ~> Reporter m e i
liftValidator (Validator (Star r)) = Reporter (Star (r >>> un Compose >>> map R.liftV >>> Compose))

toValidator ∷ ∀ e i m. Functor m ⇒ Monoid e ⇒ Reporter m e i ~> Validator m e i
toValidator (Reporter (Star f)) = Validator (Star (f >>> un Compose >>> map R.toV >>> Compose))

liftFn ∷ ∀ e i m. Applicative m ⇒ Monoid e ⇒ (Function i) ~> Reporter m e i
liftFn f = Reporter $ Star $ f >>> pure >>> pure >>> Compose

liftFnR ∷ ∀ e i m o. Applicative m ⇒ Semigroup e ⇒ (i → R e o) → Reporter m e i o
liftFnR f = Reporter $ Star $ f >>> pure >>> Compose

liftFnMR ∷ ∀ e i m o. (i → m (R e o)) → Reporter m e i o
liftFnMR f = Reporter $ Star $ map Compose f

liftFnEither ∷ ∀ e i m o. Applicative m ⇒ Monoid e ⇒ (i → Either e o) → Reporter m e i o
liftFnEither f = liftFnR $ f >>> R.liftEither

-- | Builders which look under the hood
liftFnEitherWith ∷ ∀ e i m o. Semigroup e ⇒ Applicative m ⇒ (o → e) → (i → Either e o) → Reporter m e i o
liftFnEitherWith f g = liftFnR $ g >>> R.liftEitherWith f

-- | Building `Reporter` from `Validator` by creating report from error and from value using also an input.
liftValidatorWith ∷ ∀ e i m o r. Functor m ⇒ (Tuple i e → r) → (Tuple i o → r) → Validator m e i o → Reporter m r i o
liftValidatorWith f g (Validator (Star r)) = Reporter $ Star $ \i →
  (r >>> un Compose >>> map (R.liftVWith (f <<< Tuple i) (g <<< Tuple i)) >>> Compose $ i)

-- | Building `Reporter` from `Validator` by creating report from error but leaving error/report type untouched.
liftValidatorWith' ∷ ∀ e i m o. Functor m ⇒ (Tuple i o → e) → Validator m e i o → Reporter m e i o
liftValidatorWith' f (Validator (Star r)) = Reporter $ Star $ \i →
  (r >>> un Compose >>> map (R.liftVWith' (f <<< Tuple i)) >>> Compose $ i)

liftValidatorWithM ∷ ∀ e i m o r. Monad m ⇒ (Tuple i e → m r) → (Tuple i o → m r) → Validator m e i o → Reporter m r i o
liftValidatorWithM f g (Validator (Star s)) = Reporter $ Star $ \i → Compose do
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
