module Data.Validation.Polyform.Http where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Data.Array (catMaybes)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.List (List, singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Star (Star(..))
import Data.StrMap (StrMap, empty, fromFoldable, lookup)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(Tuple))
import Data.Validation.Polyform.Prim (V(..), Validation(..), pureV)
import Data.Variant (Variant, inj)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

data Form field = Form (StrMap String) (Array field)
derive instance genericForm ∷ Generic (Form field) _
derive instance functorForm ∷ Functor Form

-- | HTTP query representation
type FieldQuery = Array (Maybe String)
type Query = StrMap FieldQuery

-- XXX: Add newtype wrapper - we need different Alt instance which
-- is not monoid based on left values
type FieldValidation m e a b = Star (ExceptT e m) a b
type FieldValidation' m e b = FieldValidation m e FieldQuery b

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

nonEmptyScalar' ∷ ∀ e m v. (Monad m) ⇒ FieldValidation' m (NonEmtpyScalarErr e) String
nonEmptyScalar' =
  catMaybesV >>> scalar' >>> (tag (SProxy ∷ SProxy "nonEmptyScalar") (Star $ ExceptT <<< pure <<< (\s →
    if s == mempty
      then Left unit
      else Right s)))

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


type FormValidation m field a b = Validation m (Form field) a b
type FormValidation' m field b = FormValidation m field Query b

newtype FormField m a b field = FormField (FormValidation m field a b)
derive instance newtypeFormField ∷ Newtype (FormField m a b field) _
instance functorFormField ∷ (Functor m) ⇒ Functor (FormField m a b) where
  map f (FormField (Validation v)) =
    FormField (Validation ((lmap (f <$> _) <$> _) <$> v))

mapField ∷ ∀ a b field field' m
  . (Functor m)
  ⇒ (field → field')
  → FormValidation m field a b
  → FormValidation m field' a b
mapField f = unwrap <<< map f <<< FormField

type Input e a =
  { name ∷ String
  , value ∷ Either e a
  }

inputValidation ∷ ∀ e m v. (Monad m) ⇒ String  → FieldValidation' m e v → FormValidation' m (Input e v) v
inputValidation name validation = Validation $ \query → do
  let
    raw = fromMaybe [] (lookup name query)
  r ← runExceptT (unwrap validation raw)
  pure $ case r of
    Left e → Invalid (Form empty [{name, value: Left e}])
    Right v → Valid (Form empty [{name, value: Right v}]) v

optInputValidation ∷ ∀ e m v. (Monad m) ⇒ String → FieldValidation' m e v → FormValidation' m (Input e (Maybe v)) (Maybe v)
optInputValidation name validation = Validation $ \query → do
  let
    raw = fromMaybe [] (lookup name query)
  e ← runExceptT (unwrap emptyOrMissing raw)
  r ← runExceptT (unwrap validation raw)
  pure $ case e, r of
    Right _, _ → Valid (Form empty [{name, value: Right Nothing}]) Nothing
    _, Left e → Invalid (Form empty [{name, value: Left e}])
    _, Right v → Valid (Form empty [{name, value: Right (Just v)}]) (Just v)

input ∷ ∀ e m. (Monad m) ⇒ String → FormValidation' m (Input (NonEmtpyScalarErr e) String) String
input name = inputValidation name nonEmptyScalar'

optInput ∷ ∀ e m. (Monad m) ⇒ String → FormValidation' m (Input (ScalarErr String e) (Maybe String)) (Maybe String)
optInput name = optInputValidation name (catMaybesV >>> scalar')


number name = inputValidation name (nonEmptyScalar' >>> int')

optNumber name = optInputValidation name (nonEmptyScalar' >>> int')

-- | You should convert opt to your desired label before passing it renderer
type Select e opt =
  { name ∷ String
  , options ∷ List (Tuple String opt)
  , value ∷ Either e String
  }

class ToOptionValue opt where
  toOptionValue ∷ opt → String

instance toOptionsConstructor ∷ (IsSymbol name) ⇒ ToOptionValue (Constructor name NoArguments) where
  toOptionValue (Constructor NoArguments) = reflectSymbol (SProxy ∷ SProxy name)

instance toOptionsSum ∷ (ToOptionValue a, ToOptionValue b) ⇒ ToOptionValue (Sum a b) where
  toOptionValue (Inl v) = toOptionValue v
  toOptionValue (Inr v) = toOptionValue v

class AsOptions opt where
  asOptions ∷ (Proxy opt) → (List (Tuple String opt))

  asRepParser ∷ ∀ m. (Monad m) ⇒ (Proxy opt) → FieldValidation m String String opt

instance asOptionsSum ∷ (AsOptions a, AsOptions b) ⇒ AsOptions (Sum a b) where
  asOptions _ =
    map (Inl <$> _) (asOptions (Proxy ∷ Proxy a))
    <> map (Inr <$> _) (asOptions (Proxy ∷ Proxy b))

  asRepParser _ =
    (Inl <$> asRepParser (Proxy ∷ Proxy a)) <|> (Inr <$> asRepParser (Proxy ∷ Proxy b))

instance asOptionsConstructor ∷ (IsSymbol name) ⇒ AsOptions (Constructor name NoArguments) where
  asOptions _ =
    singleton (Tuple value option)
   where
    option = ((Constructor NoArguments) ∷ Constructor name NoArguments)
    value = reflectSymbol (SProxy ∷ SProxy name)

  asRepParser _ =
    Star (\s → if s == value
      then (pure ((Constructor NoArguments) ∷ Constructor name NoArguments))
      else (ExceptT <<< pure $ Left "error"))
   where
    value = reflectSymbol (SProxy ∷ SProxy name)


toOptionValue' ∷ ∀ opt optRep. (Generic opt optRep) ⇒ (ToOptionValue optRep) ⇒ opt → String
toOptionValue' v = toOptionValue (from v)

asOptions' ∷ ∀ a aRep. (Generic a aRep) ⇒ (AsOptions aRep) ⇒ Proxy a → List (Tuple String a)
asOptions' _ = map (to <$> _) (asOptions (Proxy ∷ Proxy aRep))

asParser ∷ ∀ a aRep m. (Monad m) ⇒ (Generic a aRep) ⇒ (AsOptions aRep) ⇒ Proxy a → FieldValidation m String String a
asParser _ = to <$> (asRepParser (Proxy ∷ Proxy aRep))

-- --
-- -- instance genericEnumNoArguments :: GenericEnum NoArguments where
-- --   toOptionValue = []
-- 
-- 
