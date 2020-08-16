module Polyform.Validator
  ( ask
  , bimapM
  , bimapMWithInput
  , bimapValidator
  , check
  , checkM
  , invalidate
  , liftFn
  , liftFnEither
  , liftFnM
  , liftFnMV
  , liftFnMaybe
  , liftFnMMaybe
  , liftFnV
  , hoist
  , lmapM
  , lmapMWithInput
  , lmapValidator
  , runValidator
  -- , valid
  , Validator(..)
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Either (Either(..), either, note)
import Data.Functor.Compose (Compose(..), bihoistCompose)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..), invalid, unV)

newtype Validator m e i o = Validator (Star (Compose m (V e)) i o)
derive instance newtypeValidator ∷ Newtype (Validator m r i o) _
derive newtype instance functorValidator ∷ (Applicative m) ⇒ Functor (Validator m e i)
derive newtype instance applyValidator ∷ (Applicative m, Semigroup e) ⇒ Apply (Validator m e i)
derive newtype instance applicativeValidator ∷ (Applicative m, Semigroup e) ⇒ Applicative (Validator m e i)
derive newtype instance profunctorValidator ∷ Functor m ⇒ Profunctor (Validator m e)
derive newtype instance choiceValidator ∷ (Semigroup e, Applicative m) ⇒ Choice (Validator m e)
derive newtype instance strongValidator ∷ (Monad m, Semigroup e) ⇒ Strong (Validator m e)

instance altValidator ∷ (Semigroup e, Monad m) ⇒ Alt (Validator m e i) where
  alt (Validator (Star v1)) (Validator (Star v2)) = Validator $ Star \i → Compose do
    let
      Compose r1 = v1 i
    r1 >>= case _ of
      V (Right o1) → pure $ pure o1
      V (Left e1) → do
        let Compose r2 = v2 i
        r2 >>= case _ of
          V (Right o2) → pure $ pure o2
          V (Left e2) → pure $ invalid $ e1 <> e2

instance plusValidator ∷ (Monad m, Monoid e) ⇒ Plus (Validator m e i) where
  empty = Validator <<< Star <<< const <<< Compose <<< pure $ invalid mempty

instance semigroupValidator ∷ (Apply m, Semigroup e, Semigroup o) ⇒ Semigroup (Validator m e i o) where
  append (Validator v1) (Validator v2) = Validator ((<>) <$> v1 <*> v2)

instance monoidValidator ∷ (Applicative m, Monoid e, Monoid o) ⇒ Monoid (Validator m e i o) where
  mempty = Validator <<< Star <<< const <<< Compose <<< pure $ mempty

instance semigroupoidValidator ∷ (Monad m, Semigroup e) ⇒ Semigroupoid (Validator m e) where
  compose (Validator (Star v2)) (Validator (Star v1)) =
    Validator $ Star $ \i → Compose do
      let
        Compose res = v1 i
      res' ← res
      unV (pure <<< invalid) (\b → let Compose b' = v2 b in b') res'

instance categoryValidator ∷ (Monad m, Semigroup e) ⇒ Category (Validator m e) where
  identity = Validator <<< Star $ Compose <<< pure <<< pure

ask ∷ ∀ i m e. Monad m ⇒ Semigroup e ⇒ Validator m e i i
ask = liftFn identity

runValidator ∷ ∀ i m o e. Validator m e i o → (i → m (V e o))
runValidator = map unwrap <<< unwrap <<< unwrap

-- | These hoists set is used to lift functions into Validator

liftFn ∷ ∀ e i m o. Applicative m ⇒ Semigroup e ⇒ (i → o) → Validator m e i o
liftFn f = Validator $ Star $ f >>> pure >>> pure >>> Compose

liftFnV ∷ ∀ e i m o. Applicative m ⇒ (i → V e o) → Validator m e i o
liftFnV f = Validator $ Star $ f >>> pure >>> Compose

liftFnM ∷ ∀ e i m o. Applicative m ⇒ Semigroup e ⇒ (i → m o) → Validator m e i o
liftFnM f = Validator $ Star (map Compose $ map pure <$> f)

liftFnMV ∷ ∀ e i m o. (i → m (V e o)) → Validator m e i o
liftFnMV f = Validator $ Star $ map Compose f

-- | TODO: Drop it - its redundant
liftFnEither ∷ ∀ e i m o. Applicative m ⇒ Semigroup e ⇒ (i → Either e o) → Validator m e i o
liftFnEither f = liftFnV $ f >>> either invalid pure

liftFnMaybe ∷ ∀ e i m o. Applicative m ⇒ (i → e) → (i → Maybe o) → Validator m e i o
liftFnMaybe err f = liftFnV $ \i → V (note (err i) (f i))

liftFnMMaybe ∷ ∀ e i m o. Applicative m ⇒ (i → m e) → (i → m (Maybe o)) → Validator m e i o
liftFnMMaybe err f = liftFnMV \i → ado
  r ← f i
  e ← err i
  in
    V (note e r)

invalidate ∷ ∀ e i o m. Applicative m ⇒ (i → e) → Validator m e i o
invalidate inv = liftFnMaybe inv (const Nothing)

check ∷ ∀ e i m. Applicative m ⇒ Semigroup e ⇒ (i → e) → (i → Boolean) → Validator m e i i
check e c = liftFnV \i → if c i
  then pure i
  else invalid (e i)

checkM ∷ ∀ e i m. Monad m ⇒ Semigroup e ⇒ (i → e) → (i → m Boolean) → Validator m e i i
checkM e c = liftFnMV \i → c i >>= if _
  then pure $ pure i
  else pure $ invalid (e i)

-- | Lifts natural transformation so it hoists internal validator functor.
hoist ∷ ∀ e i n m. Functor m ⇒ m ~> n → Validator m e i ~> Validator n e i
hoist n (Validator (Star v)) = Validator $ Star (map (bihoistCompose n identity) v)

-- | Provides access to validation result
-- | so you can `bimap` over `e` and `b` type in resulting `V e b`.
newtype BifunctorValidator m i e o = BifunctorValidator (Validator m e i o)
derive instance newtypeBifunctorValidator ∷ Newtype (BifunctorValidator m i e o) _

instance bifunctorBifunctorValidator ∷ Monad m ⇒ Bifunctor (BifunctorValidator m i) where
  bimap l r (BifunctorValidator (Validator (Star f))) = BifunctorValidator $ Validator $ Star $ \i → Compose do
    let
      Compose v = f i
    map (bimap l r) v

bimapValidator ∷ ∀ i e e' m o o'
  . (Monad m)
  ⇒ (e → e')
  → (o → o')
  → Validator m e i o
  → Validator m e' i o'
bimapValidator l r = unwrap <<< bimap l r <<< BifunctorValidator

lmapValidator ∷ ∀ e e' i m o. Monad m ⇒ (e → e') → Validator m e i o → Validator m e' i o
lmapValidator l = unwrap <<< lmap l <<< BifunctorValidator

lmapM ∷ ∀ i m e e'. Monad m ⇒ (e → m e') → Validator m e i ~> Validator m e' i
lmapM f v = liftFnMV $ runValidator v >=> case _ of
  V (Left e) → invalid <$> f e
  V (Right a) → pure $ V (Right a)

lmapMWithInput ∷ ∀ i m e e'. Monad m ⇒ ((Tuple i e) -> m e') → Validator m e i ~> Validator m e' i
lmapMWithInput f v = liftFnMV $ \i → runValidator v i >>= case _ of
  V (Left e) → invalid <$> f (Tuple i e)
  V (Right a) → pure $ V (Right a)

bimapM
  ∷ ∀ e e' i m o o'. Monad m
  ⇒ Semigroup e'
  ⇒ (e → m e')
  → (o → m o')
  → Validator m e i o
  → Validator m e' i o'
bimapM l r v = liftFnM r  <<< lmapM l v

bimapMWithInput
  ∷ ∀ e e' i m o o'. Monad m
  ⇒ Semigroup e'
  ⇒ ((Tuple i e) → m e')
  → ((Tuple i o) → m o')
  → Validator m e i o
  → Validator m e' i o'
bimapMWithInput f g v = liftFnMV \i → runValidator v i >>= case _ of
  V (Left e) → invalid <$> f (Tuple i e)
  V (Right o) → pure <$> g (Tuple i o)
