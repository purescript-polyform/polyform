module Data.Validation.Polyform.Validation.Field where

import Prelude hiding (not)

import Control.Alt (class Alt)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (class MonadError, ExceptT(ExceptT), catchError, runExceptT)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array (uncons)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), note)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Star (Star(Star))
import Data.Variant (Variant, inj, on)
import Type.Prelude (class IsSymbol, SProxy(SProxy))


-- | This is Star over (ExceptT e m) with + MonadAsk and MonadThrow
-- | maybe we should drop this and use just (Star (ExceptT e m) a b)
newtype FieldValidation m e a b = FieldValidation (Star (ExceptT e m) a b)
derive instance newtypeFieldValidation ∷ Newtype (FieldValidation m e a b) _
derive instance functorFieldValidation ∷ (Functor m) ⇒ Functor (FieldValidation m e a)
derive newtype instance applyFieldValidation ∷ (Monad m) ⇒ Apply (FieldValidation m e a)
derive newtype instance applicativeFieldValidation ∷ (Monad m) ⇒ Applicative (FieldValidation m e a)
derive newtype instance bindValidation ∷ (Monad m) ⇒ Bind (FieldValidation m e a)
derive newtype instance semigroupoidFieldValidation ∷ (Monad m) ⇒ Semigroupoid (FieldValidation m e)
derive newtype instance categoryValidation ∷ (Monad m) ⇒ Category (FieldValidation m e)
instance monadFieldValidation ∷ (Monad m) ⇒ Monad (FieldValidation m e a)
derive newtype instance altFieldValidation ∷ (Monad m, Semigroup e) ⇒ Alt (FieldValidation m e a)
derive newtype instance profunctorFIeldValidation ∷ (Functor m) ⇒ Profunctor (FieldValidation m e)
derive newtype instance choiceFieldValidation ∷ (Monad m) ⇒ Choice (FieldValidation m e)

instance monadAskFieldValidation ∷ (Monad m) ⇒ MonadAsk a (FieldValidation m e a) where
  ask = FieldValidation $ Star (\i → pure i)

instance monadThrowFieldValidation ∷ (Monad m) ⇒ MonadThrow e (FieldValidation m e a) where
  throwError e = FieldValidation $ Star (\_ → throwError e)

instance monadCatchFieldValidation ∷ (Monad m) ⇒ MonadError e (FieldValidation m e a) where
  catchError (FieldValidation (Star fm)) h = FieldValidation $ Star \i →
    catchError (fm i) (\e → let FieldValidation (Star fa) = h e in fa i)

bimapResult ∷ ∀ a b b' e e' m
  . (Monad m)
  ⇒ (e → e')
  → (b → b')
  → FieldValidation m e a b
  → FieldValidation m e' a b'
bimapResult l r = mapResult (bimap l r)

mapResult ∷ ∀ a b b' e e' m. (Monad m) ⇒ (Either e b → Either e' b') → FieldValidation m e a b → FieldValidation m e' a b'
mapResult f (FieldValidation (Star v)) =
  FieldValidation $ Star $ \i → ExceptT $ do
    r ← runExceptT (v i)
    pure (f r)

withException ∷ ∀ a b e e' m. (Monad m) ⇒ (e → e') → FieldValidation m e a b → FieldValidation m e' a b
withException f = mapResult (lmap f)

runFieldValidation ∷ ∀ a b e m. FieldValidation m e a b → (a → ExceptT e m b)
runFieldValidation = unwrap <<< unwrap

tag :: forall a b e m p r r'
  . RowCons p e r r'
  ⇒ Monad m
  ⇒ IsSymbol p
  ⇒ SProxy p
  → FieldValidation m e a b
  → FieldValidation m (Variant r') a b
tag p v =
  withException (inj p) v

pureV ∷ ∀ a b e m. (Monad m) ⇒ (a → b) → FieldValidation m e a b
pureV f = do
  a ← ask
  pure $ f a

validate ∷ ∀ a e b m. (Monad m) ⇒ (a → Either e b) → FieldValidation m e a b
validate f = do
  i ← ask
  case f i of
    Left e → throwError e
    Right r → pure r

validateM ∷ ∀ a e b m. (Monad m) ⇒ (a → m (Either e b)) → FieldValidation m e a b
validateM f =
  FieldValidation $ Star $ \a → ExceptT $ f a

check ∷ ∀ a m. (Monad m) ⇒ (a → Boolean) → FieldValidation m a a a
check f = validate $ \i →
  if f i
    then Right i
    else Left i

checkM ∷ ∀ a m. (Monad m) ⇒ (a → m Boolean) → FieldValidation m a a a
checkM f = validateM $ \a → do
    r ← f a
    pure $ if r
      then Left a
      else Right a

_required = SProxy ∷ SProxy "required"

required ∷ ∀ a m. (Monad m) ⇒ FieldValidation m Unit (Array a) (NonEmpty Array a)
required = validate $ case _ of
  [] → Left unit
  arr → case uncons arr of
    Nothing → Left unit
    Just { head, tail } → Right (NonEmpty head tail)

required' ∷ ∀ a e m. (Monad m) ⇒ FieldValidation m (Variant (required ∷ Unit | e)) (Array a) (NonEmpty Array a)
required' = tag _required required

opt
  ∷  ∀ i e err m o
  . Monad m
  ⇒ FieldValidation m (Variant ( required ∷ e | err )) i o
  → FieldValidation m (Variant err) i (Maybe o)
opt v =
  mapResult dropMissing v
 where
  dropMissing (Left e) = on _required (const (Right Nothing)) Left e
  dropMissing (Right r) = Right (Just r)

_scalar = (SProxy ∷ SProxy "scalar")

scalar ∷ ∀ a m. (Monad m) ⇒ FieldValidation m (NonEmpty Array a) (NonEmpty Array a) a
scalar = validate $ case _ of
  NonEmpty a [] → Right a
  arr → Left arr

scalar' ∷ ∀ a e m. (Monad m) ⇒ FieldValidation m (Variant (scalar ∷ NonEmpty Array a | e)) (NonEmpty Array a) a
scalar' = tag _scalar scalar


-- XXX: What kind of validators do we want to keep here...

_int = SProxy ∷ SProxy "int"

int ∷ ∀ m. (Monad m) ⇒ FieldValidation m String String Int
int = validate $ note <*> fromString

int' ∷ ∀ m e. (Monad m) ⇒ FieldValidation m (Variant (int ∷ String | e)) String Int
int' = tag _int int



nonEmpty' ∷ ∀ a e m
  . (Monad m)
  ⇒ (Monoid a)
  ⇒ (Eq a)
  ⇒ FieldValidation m (Variant (nonEmpty ∷ a | e)) a a
nonEmpty' = tag (SProxy ∷ SProxy "nonEmpty") (not empty)

empty ∷ ∀ a m. (Monad m) ⇒ (Monoid a) ⇒ (Eq a) ⇒ FieldValidation m a a a
empty  = check (_ == mempty)

empty' ∷ ∀ a e m
 . (Monad m)
 ⇒ (Monoid a)
 ⇒ (Eq a)
 ⇒ FieldValidation m (Variant (empty ∷ a | e)) a a
empty' = tag (SProxy ∷ SProxy "empty") empty

not ∷ ∀ a e i m. (Monad m) ⇒ FieldValidation m e i a → FieldValidation m a i e
not (FieldValidation (Star f))  = FieldValidation $ Star \i → ExceptT $ do
  r ← runExceptT (f i)
  pure $ case r of
    Right v → Left v
    Left e → Right e


missing ∷ ∀ a m. (Monad m) ⇒ FieldValidation m (Array a) (Array a) Unit
missing = validate $ case _ of
  [] → Right unit
  arr → Left arr

missing' ∷ ∀ a e m
 . (Monad m)
 ⇒ FieldValidation m (Variant (missing ∷ Array a | e)) (Array a) Unit
missing' = tag (SProxy ∷ SProxy "missing") missing


emptyOrMissing ∷ ∀ a m
  . (Monad m)
  ⇒ (Monoid a)
  ⇒ (Eq a)
  ⇒ FieldValidation m (Array a) (Array a) Unit
emptyOrMissing = unit <$ (check (case _ of
  [] → true
  [a] → a == mempty
  otherwise → false))

