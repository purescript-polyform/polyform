module Polyform.Validator where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Validation.Semigroup (V, invalid, unV)

valid ∷ ∀ a e. Semigroup e ⇒ a → V e a
valid = pure

newtype Validator m r a b = Validator (a → m (V r b))
derive instance newtypeValidator ∷ Newtype (Validator m r a b) _
derive instance functorValidator ∷ (Functor m) ⇒ Functor (Validator m r a)

instance applyValidator ∷ (Semigroup r, Monad m) ⇒ Apply (Validator m r a) where
  apply vf va = Validator $ \i → do
    vf' ← unwrap vf i
    va' ← unwrap va i
    pure $ vf' <*> va'

instance applicativeValidator ∷ (Monoid r, Monad m) ⇒ Applicative (Validator m r a) where
  pure = Validator <<< const <<< pure <<< pure

instance altValidator ∷ (Semigroup e, Monad m) ⇒ Alt (Validator m e a) where
  alt (Validator v1) (Validator v2) = Validator \a → do
    res ← v1 a
    unV
      (\_ → v2 a)
      (pure <<< pure)
      res

instance plusValidator ∷ (Monad m, Monoid r) ⇒ Plus (Validator m r a) where
  empty = Validator <<< const <<< pure $ invalid mempty

instance semigroupValidator ∷ (Semigroup (m (V e b))) ⇒ Semigroup (Validator m e a b) where
  append (Validator v1) (Validator v2) = Validator (\a → v1 a <> v2 a)

instance monoidValidator ∷ (Applicative m, Monoid e, Monoid b, Semigroup (m (V e b))) ⇒ Monoid (Validator m e a b) where
  mempty = Validator <<< const <<< pure $ mempty

instance semigroupoidValidator ∷ (Monad m, Semigroup e) ⇒ Semigroupoid (Validator m e) where
  compose (Validator v2) (Validator v1) =
    Validator $ \a → do
      res ← v1 a
      flip (unV (pure <<< invalid)) res $
        \b → do
          res' ← v2 b
          unV
            (pure <<< invalid)
            (pure <<< pure)
            res'

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

ask ∷ ∀ a r m. Monad m ⇒ Monoid r ⇒ Validator m r a a
ask = Validator (\a → pure (pure a))

runValidator ∷ ∀ a b r m. Validator m r a b → (a → m (V r b))
runValidator = unwrap

hoistFn ∷ ∀ a b m r. Monad m ⇒ Monoid r ⇒ (a → b) → Validator m r a b
hoistFn f = Validator $ f >>> pure >>> pure

hoistFnV ∷ ∀ a b m r. Monad m ⇒ Monoid r ⇒ (a → V r b) → Validator m r a b
hoistFnV f = Validator $ f >>> pure

hoistFnMV ∷ ∀ a b m r. Monad m ⇒ Monoid r ⇒ (a → m (V r b)) → Validator m r a b
hoistFnMV f = Validator f

-- | Provides access to validation result
-- | so you can `bimap` over `e` and `b` type in resulting `V e b`.
newtype BifunctorValidator m a e b = BifunctorValidator (Validator m e a b)
derive instance newtypeBifunctorValidator ∷ Newtype (BifunctorValidator m a e b) _

instance bifunctorBifunctorValidator ∷ Monad m ⇒ Bifunctor (BifunctorValidator m a) where
  bimap l r (BifunctorValidator (Validator f)) = BifunctorValidator $ Validator $ \a → do
    v ← f a
    pure $ bimap l r v

bimapValidator ∷ ∀ a b b' e e' m
  . (Monad m)
  ⇒ (e → e')
  → (b → b')
  → Validator m e a b
  → Validator m e' a b'
bimapValidator l r = unwrap <<< bimap l r <<< BifunctorValidator

lmapValidator ∷ ∀ a b m e e'. Monad m ⇒ (e → e') → Validator m e a b → Validator m e' a b
lmapValidator l = unwrap <<< lmap l <<< BifunctorValidator

rmapValidator ∷ ∀ a b b' m r. Monad m ⇒ (b → b') → Validator m r a b → Validator m r a b'
rmapValidator l = unwrap <<< rmap l <<< BifunctorValidator
