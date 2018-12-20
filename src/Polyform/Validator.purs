module Polyform.Validator where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Either (Either(..), either)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Validation.Semigroup (V, invalid, unV)
import Polyform.Reporter (Reporter(..), toV)

valid ∷ ∀ a e. Semigroup e ⇒ a → V e a
valid = pure

newtype Validator m e i o = Validator (i → m (V e o))
derive instance newtypeValidator ∷ Newtype (Validator m r i o) _
derive instance functorValidator ∷ (Functor m) ⇒ Functor (Validator m r i)

instance applyValidator ∷ (Semigroup r, Monad m) ⇒ Apply (Validator m r i) where
  apply vf va = Validator $ \i → do
    vf' ← unwrap vf i
    va' ← unwrap va i
    pure $ vf' <*> va'

instance applicativeValidator ∷ (Monoid r, Monad m) ⇒ Applicative (Validator m r i) where
  pure = Validator <<< const <<< pure <<< pure

instance altValidator ∷ (Semigroup e, Monad m) ⇒ Alt (Validator m e i) where
  alt (Validator v1) (Validator v2) = Validator \i → do
    res ← v1 i
    unV
      (\_ → v2 i)
      (pure <<< pure)
      res

instance plusValidator ∷ (Monad m, Monoid e) ⇒ Plus (Validator m e i) where
  empty = Validator <<< const <<< pure $ invalid mempty

instance semigroupValidator ∷ (Apply m, Semigroup e, Semigroup o) ⇒ Semigroup (Validator m e i o) where
  append (Validator v1) (Validator v2) = Validator (\i → (<>) <$> v1 i <*> v2 i)

instance monoidValidator ∷ (Applicative m, Monoid e, Monoid o) ⇒ Monoid (Validator m e i o) where
  mempty = Validator <<< const <<< pure $ mempty

instance semigroupoidValidator ∷ (Monad m, Semigroup e) ⇒ Semigroupoid (Validator m e) where
  compose (Validator v2) (Validator v1) =
    Validator $ \i → do
      res ← v1 i
      unV (pure <<< invalid) v2 res

instance categoryValidator ∷ (Monad m, Monoid e) ⇒ Category (Validator m e) where
  identity = Validator $ pure <<< pure

instance profunctorValidator ∷ (Monad m, Monoid e) ⇒ Profunctor (Validator m e) where
  dimap l r v = (hoistFn l) >>> v >>> (hoistFn r)

instance choiceValidator ∷ (Monad m, Monoid r) ⇒ Choice (Validator m r) where
  left v = Validator (case _ of
    Left i → map Left <$> runValidator v i
    Right r → pure (pure $ Right r))

  right v = Validator (case _ of
    Right i → map Right <$> runValidator v i
    Left l → pure (pure $ Left l))

ask ∷ ∀ i m r. Monad m ⇒ Monoid r ⇒ Validator m r i i
ask = Validator (\i → pure (pure i))

runValidator ∷ ∀ i m o r. Validator m r i o → (i → m (V r o))
runValidator = unwrap

hoistFn ∷ ∀ i m o r. Monad m ⇒ Monoid r ⇒ (i → o) → Validator m r i o
hoistFn f = Validator $ f >>> pure >>> pure

hoistFnV ∷ ∀ i m o r. Monad m ⇒ Monoid r ⇒ (i → V r o) → Validator m r i o
hoistFnV f = Validator $ f >>> pure

hoistFnMV ∷ ∀ i m o r. Monad m ⇒ Monoid r ⇒ (i → m (V r o)) → Validator m r i o
hoistFnMV f = Validator f

hoistFnEither ∷ ∀ e i m o. Monad m ⇒ Monoid e ⇒ (i → Either e o) → Validator m e i o
hoistFnEither f = hoistFnV $ f >>> either invalid pure

hoistReporter ∷ ∀ e i m o. Monad m ⇒ Monoid e ⇒ Reporter m e i o → Validator m e i o
hoistReporter (Reporter f) = Validator (f >>> map toV)

-- | Provides access to validation result
-- | so you can `bimap` over `e` and `b` type in resulting `V e b`.
newtype BifunctorValidator m i e o = BifunctorValidator (Validator m e i o)
derive instance newtypeBifunctorValidator ∷ Newtype (BifunctorValidator m i e o) _

instance bifunctorBifunctorValidator ∷ Monad m ⇒ Bifunctor (BifunctorValidator m i) where
  bimap l r (BifunctorValidator (Validator f)) = BifunctorValidator $ Validator $ \i → do
    v ← f i
    pure $ bimap l r v

bimapValidator ∷ ∀ i e e' m o o'
  . (Monad m)
  ⇒ (e → e')
  → (o → o')
  → Validator m e i o
  → Validator m e' i o'
bimapValidator l r = unwrap <<< bimap l r <<< BifunctorValidator

lmapValidator ∷ ∀ i e e' m o. Monad m ⇒ (e → e') → Validator m e i o → Validator m e' i o
lmapValidator l = unwrap <<< lmap l <<< BifunctorValidator

rmapValidator ∷ ∀ i o o' m r. Monad m ⇒ (o → o') → Validator m r i o → Validator m r i o'
rmapValidator l = unwrap <<< rmap l <<< BifunctorValidator
