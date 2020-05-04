module Polyform.Validator where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Trans.Class (lift) as Trans.Class
import Control.Plus (class Plus)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Either (Either(..), either, note)
import Data.Functor.Compose (Compose(..), bihoistCompose)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (class Strong)
import Data.Validation.Semigroup (V(..), invalid, unV)

newtype Validator m e i o = Validator (Star (Compose m (V e)) i o)
derive instance newtypeValidator ∷ Newtype (Validator m r i o) _
derive newtype instance functorValidator ∷ (Applicative m, Semigroup e) ⇒ Functor (Validator m e i)
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
ask = hoistFn identity

runValidator ∷ ∀ i m o e. Validator m e i o → (i → m (V e o))
runValidator = map unwrap <<< unwrap <<< unwrap

-- | These hoists set is used to lift functions into Validator

hoistFn ∷ ∀ e i m o. Applicative m ⇒ Semigroup e ⇒ (i → o) → Validator m e i o
hoistFn f = Validator $ Star $ f >>> pure >>> pure >>> Compose

hoistFnV ∷ ∀ e i m o. Applicative m ⇒ Semigroup e ⇒ (i → V e o) → Validator m e i o
hoistFnV f = Validator $ Star $ f >>> pure >>> Compose

hoistFnM ∷ ∀ e i m o. Applicative m ⇒ Semigroup e ⇒ (i → m o) → Validator m e i o
hoistFnM f = Validator $ Star (map Compose $ map valid <$> f)

hoistFnMV ∷ ∀ e i m o. (i → m (V e o)) → Validator m e i o
hoistFnMV f = Validator $ Star $ map Compose f

hoistFnEither ∷ ∀ e i m o. Applicative m ⇒ Semigroup e ⇒ (i → Either e o) → Validator m e i o
hoistFnEither f = hoistFnV $ f >>> either invalid pure

hoistFnMaybe ∷ ∀ e i m o. Applicative m ⇒ Semigroup e ⇒ e → (i → Maybe o) → Validator m e i o
hoistFnMaybe msg f = hoistFnEither $ f >>> note msg

hoist ∷ ∀ e i n m o. Functor m ⇒ (m ~> n) → Validator m e i o → Validator n e i o
hoist n (Validator (Star v)) = Validator $ Star (map (bihoistCompose n identity) v)

lift ∷ ∀ e i o m t. MonadTrans t ⇒ Monad m ⇒ Validator m e i o → Validator (t m) e i o
lift = hoist Trans.Class.lift

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

rmapValidator ∷ ∀ e i o o' m. Monad m ⇒ (o → o') → Validator m e i o → Validator m e i o'
rmapValidator l = unwrap <<< rmap l <<< BifunctorValidator

optional ∷ ∀ e i o m. Monad m ⇒ Semigroup e ⇒ Validator m e i o → Validator m e i (Maybe o)
optional v = hoistFnMV \i → do
  r ← runValidator v i
  pure $ unV (const $ pure Nothing) (pure <<< Just) r

valid ∷ ∀ a e. Semigroup e ⇒ a → V e a
valid = pure

toStarExceptT ∷ ∀ e i m o. Functor m ⇒ Validator m e i o → Star (ExceptT e m) i o
toStarExceptT (Validator (Star f)) = Star (map (unwrap >>> map unwrap >>> ExceptT) f)

-- | It is often the case that after successfull chain of validation we want
-- | to drop to "non semigroup" error representation like `Variant` etc.
lmapToStarExceptT ∷ ∀ e e' i m o. Monad m ⇒ (e → e') → Validator m e i o → Star (ExceptT e' m) i o
lmapToStarExceptT withE = toStarExceptT <<< lmapValidator withE

fromStarExceptT ∷ ∀ e i m o. Functor m ⇒ Star (ExceptT e m) i o → Validator m e i o
fromStarExceptT (Star f) = Validator $ Star (map (unwrap >>> map V >>> Compose) f)

