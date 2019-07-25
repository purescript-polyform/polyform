module Polyform.Validator where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Trans.Class (lift) as Trans.Class
import Control.Plus (class Plus)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Either (Either(..), either)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid, unV)

valid ∷ ∀ a e. Semigroup e ⇒ a → V e a
valid = pure

newtype Validator m e i o = Validator (i → m (V e o))
derive instance newtypeValidator ∷ Newtype (Validator m r i o) _
derive instance functorValidator ∷ (Functor m) ⇒ Functor (Validator m r i)

instance applyValidator ∷ (Semigroup r, Applicative m) ⇒ Apply (Validator m r i) where
  apply vf va = Validator $ \i → ado
    vf' ← unwrap vf i
    va' ← unwrap va i
    in vf' <*> va'

instance applicativeValidator ∷ (Semigroup r, Applicative m) ⇒ Applicative (Validator m r i) where
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

instance categoryValidator ∷ (Monad m, Semigroup e) ⇒ Category (Validator m e) where
  identity = Validator $ pure <<< pure

instance profunctorValidator ∷ (Monad m, Semigroup e) ⇒ Profunctor (Validator m e) where
  dimap l r v = (hoistFn l) >>> v >>> (hoistFn r)

instance choiceValidator ∷ (Monad m, Semigroup r) ⇒ Choice (Validator m r) where
  left v = Validator (case _ of
    Left i → map Left <$> runValidator v i
    Right r → pure (pure $ Right r))

  right v = Validator (case _ of
    Right i → map Right <$> runValidator v i
    Left l → pure (pure $ Left l))

instance strongValidator ∷ (Monad m, Semigroup e) ⇒ Strong (Validator m e) where
  first v = Validator (\(Tuple f s) → map (\f' → Tuple f' s) <$> runValidator v f)
  second v = Validator (\(Tuple f s) → map (\s' → Tuple f s') <$> runValidator v s)

ask ∷ ∀ i m e. Monad m ⇒ Semigroup e ⇒ Validator m e i i
ask = hoistFn identity

runValidator ∷ ∀ i m o e. Validator m e i o → (i → m (V e o))
runValidator = unwrap

-- | These hoists set is used to lift functions into Validator

hoistFn ∷ ∀ e i m o. Applicative m ⇒ Semigroup e ⇒ (i → o) → Validator m e i o
hoistFn f = Validator $ f >>> pure >>> pure

hoistFnV ∷ ∀ e i m o. Applicative m ⇒ Semigroup e ⇒ (i → V e o) → Validator m e i o
hoistFnV f = Validator $ f >>> pure

hoistFnM ∷ ∀ e i m o. Applicative m ⇒ Semigroup e ⇒ (i → m o) → Validator m e i o
hoistFnM f = Validator (map valid <$> f)

hoistFnMV ∷ ∀ e i m o. Applicative m ⇒ Semigroup e ⇒ (i → m (V e o)) → Validator m e i o
hoistFnMV f = Validator f

hoistFnEither ∷ ∀ e i m o. Applicative m ⇒ Semigroup e ⇒ (i → Either e o) → Validator m e i o
hoistFnEither f = hoistFnV $ f >>> either invalid pure

-- | Apply underling modad to underling `Applicative`
hoist ∷ ∀ e i n m o. (m ~> n) → Validator m e i o → Validator n e i o
hoist n (Validator v) = Validator (map n v)

lift ∷ ∀ e i o m t. MonadTrans t ⇒ Monad m ⇒ Validator m e i o → Validator (t m) e i o
lift (Validator v) = Validator (map Trans.Class.lift v)

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
