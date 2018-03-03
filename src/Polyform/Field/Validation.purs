module Polyform.Field.Validation where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (class MonadError, ExceptT(ExceptT), catchError, runExceptT)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array (uncons)
import Data.Bifunctor as Bifunctor
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Star (Star(Star))
import Data.Variant (Variant, inj, on)
import Type.Prelude (class IsSymbol, SProxy(..))

-- | This library assumes simple strategy for field
-- | validation which is diffrent than form processing.
-- | We are treating fields as atomic entities with
-- | simple monadic validation chain based on Either.
newtype Validation m e a b = Validation (Star (ExceptT e m) a b)
derive instance newtypeValidation ∷ Newtype (Validation m e a b) _
derive instance functorValidation ∷ (Functor m) ⇒ Functor (Validation m e a)
derive newtype instance applyValidation ∷ (Monad m) ⇒ Apply (Validation m e a)
derive newtype instance applicativeValidation ∷ (Monad m) ⇒ Applicative (Validation m e a)
derive newtype instance bindValidation ∷ (Monad m) ⇒ Bind (Validation m e a)
derive newtype instance semigroupoidValidation ∷ (Monad m) ⇒ Semigroupoid (Validation m e)
derive newtype instance categoryValidation ∷ (Monad m) ⇒ Category (Validation m e)
instance monadValidation ∷ (Monad m) ⇒ Monad (Validation m e a)
derive newtype instance altValidation ∷ (Monad m, Semigroup e) ⇒ Alt (Validation m e a)
derive newtype instance profunctorValidation ∷ (Functor m) ⇒ Profunctor (Validation m e)
derive newtype instance choiceValidation ∷ (Monad m) ⇒ Choice (Validation m e)

instance monadAskFieldValidation ∷ (Monad m) ⇒ MonadAsk a (Validation m e a) where
  ask = Validation $ Star (\i → pure i)

instance monadThrowFieldValidation ∷ (Monad m) ⇒ MonadThrow e (Validation m e a) where
  throwError e = Validation $ Star (\_ → throwError e)

instance monadCatchFieldValidation ∷ (Monad m) ⇒ MonadError e (Validation m e a) where
  catchError (Validation (Star fm)) h = Validation $ Star \i →
    catchError (fm i) (\e → let Validation (Star fa) = h e in fa i)

runValidation ∷ ∀ a b e m. Validation m e a b → (a → ExceptT e m b)
runValidation = unwrap <<< unwrap

bimap ∷ ∀ a b b' e e' m
  . Monad m
  ⇒ (e → e')
  → (b → b')
  → Validation m e a b
  → Validation m e' a b'
bimap l r = mapEither (Bifunctor.bimap l r)

mapEither
  ∷ ∀ a b b' e e' m
  . Monad m
  ⇒ (Either e b → Either e' b')
  → Validation m e a b
  → Validation m e' a b'
mapEither f (Validation (Star v)) =
  Validation $ Star $ \i → ExceptT $ do
    r ← runExceptT (v i)
    pure (f r)

withException
  ∷ ∀ a b e e' m
  . Monad m
  ⇒ (e → e')
  → Validation m e a b
  → Validation m e' a b
withException f = mapEither (Bifunctor.lmap f)

hoistPure
  ∷ ∀ a b e m
  . Monad m
  ⇒ (a → b)
  → Validation m e a b
hoistPure f = do
  a ← ask
  pure $ f a

hoistEither
  ∷ ∀ a e b m
  . Monad m
  ⇒ (a → Either e b)
  → Validation m e a b
hoistEither f = do
  i ← ask
  case f i of
    Left e → throwError e
    Right r → pure r

hoistMEither
  ∷ ∀ a e b m
  . Monad m
  ⇒ (a → m (Either e b))
  → Validation m e a b
hoistMEither f = Validation $ Star $ \a → ExceptT $ f a

tag :: forall a b e m p r r'
  . RowCons p e r r'
  ⇒ Monad m
  ⇒ IsSymbol p
  ⇒ SProxy p
  → Validation m e a b
  → Validation m (Variant r') a b
tag p v =
  withException (inj p) v


checkPure
  ∷ ∀ a m
  . Monad m
  ⇒ (a → Boolean)
  → Validation m a a a
checkPure f = hoistEither $ \i →
  if f i
    then Right i
    else Left i

checkM
  ∷ ∀ a m
  . Monad m
  ⇒ (a → m Boolean)
  → Validation m a a a
checkM f = hoistMEither $ \a → do
    r ← f a
    pure $ if r
      then Left a
      else Right a

_required = SProxy ∷ SProxy "required"

required
  ∷ ∀ a e m
  . Monad m
  ⇒ Validation
      m (Variant (required ∷ Unit | e)) (Array a) (NonEmpty Array a)
required = tag _required $ hoistEither $ case _ of
  [] → Left unit
  arr → case uncons arr of
    Nothing → Left unit
    Just { head, tail } → Right (NonEmpty head tail)

opt
  ∷  ∀ i e err m o
  . Monad m
  ⇒ Validation m (Variant ( required ∷ e | err )) i o
  → Validation m (Variant err) i (Maybe o)
opt v =
  mapEither dropMissing v
 where
  dropMissing (Left e) = on _required (const (Right Nothing)) Left e
  dropMissing (Right r) = Right (Just r)

_scalar = (SProxy ∷ SProxy "scalar")

scalar ∷ ∀ a e m. (Monad m) ⇒ Validation m (Variant (scalar ∷ NonEmpty Array a | e)) (NonEmpty Array a) a
scalar = tag _scalar $ hoistEither $ case _ of
  NonEmpty a [] → Right a
  arr → Left arr
