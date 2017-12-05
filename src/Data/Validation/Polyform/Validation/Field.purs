module Data.Validation.Polyform.Validation.Field where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Data.Array (catMaybes)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.List (List, any, singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Star (Star(..))
import Data.Record (insert, set)
import Data.StrMap (StrMap, empty, fromFoldable, lookup)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(Tuple))
import Data.Variant (Variant, inj)
import Type.Prelude (class IsSymbol, class RowLacks, Proxy(..), SProxy(..), reflectSymbol)


type FieldValidation m e a b = Star (ExceptT e m) a b

tag :: forall a b e m p r r'
  . RowCons p e r r'
  ⇒ Monad m
  ⇒ IsSymbol p
  ⇒ SProxy p
  → FieldValidation m e a b
  → FieldValidation m (Variant r') a b
tag p v =
  wrap ((ExceptT <<< (lmap (inj p) <$> _) <<< runExceptT) <$> unwrap v)


catMaybesV :: forall a e m. (Monad m) ⇒ FieldValidation m e (Array (Maybe a)) (Array a)
catMaybesV = Star $ ExceptT <<< pure <<< Right <<< catMaybes

scalar ∷ ∀ a m. (Monad m) ⇒ FieldValidation m (Array a) (Array a) a
scalar = Star $ \i → ExceptT <<< pure <<< s $ i
 where
  s [a] = Right a
  s arr = Left arr

type ScalarErr a e = Variant (scalar ∷ Array a | e)

scalar' ∷ ∀ a e m. (Monad m) ⇒ FieldValidation m (ScalarErr a e) (Array a) a
scalar' = tag (SProxy ∷ SProxy "scalar") scalar

type NonEmtpyScalarErr e = ScalarErr String (nonEmptyScalar ∷ Unit | e)

-- nonEmptyScalar' ∷ ∀ e m v. (Monad m) ⇒ FieldValidation' m (NonEmtpyScalarErr e) String
-- nonEmptyScalar' =
--   catMaybesV >>> scalar' >>> (tag (SProxy ∷ SProxy "nonEmptyScalar") (Star $ ExceptT <<< pure <<< (\s →
--     if s == mempty
--       then Left unit
--       else Right s)))

type IntErr e = Variant (int ∷ String | e)

int ∷ ∀ m. (Monad m) ⇒ FieldValidation m String String Int
int = Star $ \s → ExceptT <<< pure $ note s (fromString s)

int' ∷ ∀ m e. (Monad m) ⇒ FieldValidation m (IntErr e) String Int
int' = tag (SProxy ∷ SProxy "int") int

emptyScalar ∷ ∀ m s. (Monad m) ⇒ (Monoid s) ⇒ (Eq s) ⇒ FieldValidation m (Array (Maybe s)) (Array (Maybe s)) Unit
emptyScalar =
  Star $ ExceptT <<< pure <<< s
 where
  s [v] | v == Nothing = Right unit
        | v == Just mempty = Right unit
  s i = Left i

missing ∷ ∀ a m. (Monad m) ⇒ FieldValidation m (Array a) (Array a) Unit
missing =
  Star $ ExceptT <<< pure <<< s
 where
  s [] = Right unit
  s arr = Left arr

emptyOrMissing :: ∀ s m
  . Monad m
  ⇒ Monoid s
  ⇒ Eq s
  ⇒ FieldValidation m (Array (Maybe s)) (Array (Maybe s)) Unit
emptyOrMissing = emptyScalar <|> missing

emptyOrMissing' = tag (SProxy ∷ SProxy "emptyOrMissing") emptyOrMissing

