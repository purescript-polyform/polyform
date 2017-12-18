module Data.Validation.Polyform.Validation.Field where

import Prelude hiding (not)

import Control.Alt (class Alt)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (class MonadError, ExceptT(ExceptT), catchError, runExceptT, withExceptT)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Either (Either(..), note)
import Data.Int (fromString)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor.Star (Star(Star))
import Data.Variant (Variant, inj)
import Type.Prelude (class IsSymbol, SProxy(SProxy))


-- | This is Star over (ExceptT e m) with + MonadAsk and MonadThrow
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

instance monadAskFieldValidation ∷ (Monad m) ⇒ MonadAsk a (FieldValidation m e a) where
  ask = FieldValidation $ Star (\i → pure i)

instance monadThrowFieldValidation ∷ (Monad m) ⇒ MonadThrow e (FieldValidation m e a) where
  throwError e = FieldValidation $ Star (\_ → throwError e)

instance monadCatchFieldValidation ∷ (Monad m) ⇒ MonadError e (FieldValidation m e a) where
  catchError (FieldValidation (Star fm)) h = FieldValidation $ Star \i →
    catchError (fm i) (\e → let FieldValidation (Star fa) = h e in fa i)

newtype Result m a e b = Result (FieldValidation m e a b)
derive instance newtypeResult ∷ Newtype (Result m a e b) _

instance bifunctorResult ∷ (Monad m) ⇒ Bifunctor (Result m a) where
  bimap l r (Result v) =
    Result $ FieldValidation <<< Star $ map r <<< withExceptT l <<< (runFieldValidation v)

bimapResult ∷ ∀ a b b' e e' m
  . (Monad m)
  ⇒ (e → e')
  → (b → b')
  → FieldValidation m e a b
  → FieldValidation m e' a b'
bimapResult l r = unwrap <<< bimap l r <<< Result

withException ∷ ∀ a b e e' m. (Monad m) ⇒ (e → e') → FieldValidation m e a b → FieldValidation m e' a b
withException f = unwrap <<< lmap f <<< Result

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

scalar ∷ ∀ a m. (Monad m) ⇒ FieldValidation m (Array a) (Array a) a
scalar = validate $ case _ of
  [a] → Right a
  arr → Left arr

scalar' ∷ ∀ a e m. (Monad m) ⇒ FieldValidation m (Variant (scalar ∷ Array a | e)) (Array a) a
scalar' = tag (SProxy ∷ SProxy "scalar") scalar

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

int ∷ ∀ m. (Monad m) ⇒ FieldValidation m String String Int
int = validate $ note <*> fromString

int' ∷ ∀ m e. (Monad m) ⇒ FieldValidation m (Variant (int ∷ String | e)) String Int
int' = tag (SProxy ∷ SProxy "int") int

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

required ∷ ∀ a m. (Monad m) ⇒ FieldValidation m Unit (Array a) (Array a)
required = not missing

required' ∷ ∀ a e m. (Monad m) ⇒ FieldValidation m (Variant (required ∷ Unit | e)) (Array a) (Array a)
required' = tag (SProxy ∷ SProxy "required") required

newtype Last a = Last a
derive instance newtypeLast ∷ Newtype (Last a) _
instance semigroupLast ∷ Semigroup (Last a) where
  append _ l = l

emptyOrMissing ∷ ∀ a m
  . (Monad m)
  ⇒ (Monoid a)
  ⇒ (Eq a)
  ⇒ FieldValidation m (Array a) (Array a) Unit
emptyOrMissing = unit <$ (check (case _ of
  [] → true
  [a] → a == mempty
  otherwise → false))

