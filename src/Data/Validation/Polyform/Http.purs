module Data.Validation.Polyform.Http where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Data.Array (catMaybes)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity(..))
import Data.Int (fromString)
import Data.List (List, any, singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Star (Star(..), hoistStar)
import Data.Record (insert, set)
import Data.StrMap (StrMap, empty, fromFoldable, lookup)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(Tuple))
import Data.Validation.Polyform.Validation.Form (V(..), Validation(..), pureV)
import Data.Variant (Variant, inj)
import Type.Prelude (class IsSymbol, class RowLacks, Proxy(..), SProxy(..), reflectSymbol)

data Form errors field = Form errors (Array field)
derive instance genericForm ∷ Generic (Form err field) _
derive instance functorForm ∷ Functor (Form err)

instance semigroupForm ∷ (Semigroup errors) ⇒ Semigroup (Form errors field) where
  append (Form e1 fs1) (Form e2 fs2) = Form (e1 <> e2) (fs1 <> fs2)

instance monoidForm ∷ (Monoid errors) ⇒ Monoid (Form errors field) where
  mempty = Form mempty []

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


type FormValidation m err field a b = Validation m (Form err field) a b
type FormValidation' m err field b = FormValidation m err field Query b

newtype FormField m err a b field = FormField (FormValidation m err field a b)
derive instance newtypeFormField ∷ Newtype (FormField m err a b field) _
instance functorFormField ∷ (Functor m) ⇒ Functor (FormField m err a b) where
  map f (FormField (Validation v)) =
    FormField (Validation ((lmap (f <$> _) <$> _) <$> v))

mapField ∷ ∀ a b err field field' m
  . (Functor m)
  ⇒ (field → field')
  → FormValidation m err field a b
  → FormValidation m err field' a b
mapField f = unwrap <<< map f <<< FormField

type Input e a =
  { name ∷ String
  , value ∷ Either e a
  }

inputValidation ∷ ∀ e err m v. (Monad m) ⇒ Monoid err ⇒ String  → FieldValidation' m e v → FormValidation' m err (Input e v) v
inputValidation name validation = Validation $ \query → do
  let
    raw = fromMaybe [] (lookup name query)
  r ← runExceptT (unwrap validation raw)
  pure $ case r of
    Left e → Invalid (Form mempty [{name, value: Left e}])
    Right v → Valid (Form mempty [{name, value: Right v}]) v

optInputValidation ∷ ∀ e err m v. (Monad m) ⇒ (Monoid err) ⇒ String → FieldValidation' m e v → FormValidation' m err (Input e (Maybe v)) (Maybe v)
optInputValidation name validation = Validation $ \query → do
  let
    raw = fromMaybe [] (lookup name query)
  e ← runExceptT (unwrap emptyOrMissing raw)
  r ← runExceptT (unwrap validation raw)
  pure $ case e, r of
    Right _, _ → Valid (Form mempty [{name, value: Right Nothing}]) Nothing
    _, Left e → Invalid (Form mempty [{name, value: Left e}])
    _, Right v → Valid (Form mempty [{name, value: Right (Just v)}]) (Just v)

input ∷ ∀ e err m. Monad m ⇒ Monoid err ⇒ String → FormValidation' m err (Input (NonEmtpyScalarErr e) String) String
input name = inputValidation name nonEmptyScalar'

optInput ∷ ∀ e err m. Monad m ⇒ Monoid err ⇒ String → FormValidation' m err (Input (ScalarErr String e) (Maybe String)) (Maybe String)
optInput name = optInputValidation name (catMaybesV >>> scalar')


number name = inputValidation name (nonEmptyScalar' >>> int')

optNumber name = optInputValidation name (nonEmptyScalar' >>> int')

-- | You should convert opt to your desired label before passing it renderer
type Select e opt =
  { name ∷ String
  , options ∷ List (Tuple String opt)
  , value ∷ Either e String
  }

newtype Last a = Last a
derive instance newtypeLast ∷ Newtype (Last a) _
instance semigroupLast ∷ Semigroup (Last a) where
  append f _ = f

class Options opt where
  toOptionValueImpl ∷ opt → String
  optionsImpl ∷ (Proxy opt) → (List (Tuple String opt))
  -- | This `Last` wrapper is to simplify error handling
  -- | as it is has simple Semigroup instance which takes only first value.
  -- | I drop this wrapper later on.
  optionsParserImpl ∷ ∀ m. (Monad m) ⇒ (Proxy opt) → FieldValidation m (Last String) String opt

instance asOptionsSum ∷ (Options a, Options b) ⇒ Options (Sum a b) where
  toOptionValueImpl (Inl v) = toOptionValueImpl v
  toOptionValueImpl (Inr v) = toOptionValueImpl v

  optionsImpl _ =
    map (Inl <$> _) (optionsImpl (Proxy ∷ Proxy a))
    <> map (Inr <$> _) (optionsImpl (Proxy ∷ Proxy b))

  optionsParserImpl _ =
    (Inl <$> optionsParserImpl (Proxy ∷ Proxy a)) <|> (Inr <$> optionsParserImpl (Proxy ∷ Proxy b))

instance asOptionsConstructor ∷ (IsSymbol name) ⇒ Options (Constructor name NoArguments) where
  toOptionValueImpl _ = reflectSymbol (SProxy ∷ SProxy name)

  optionsImpl _ =
    singleton (Tuple value option)
   where
    option = ((Constructor NoArguments) ∷ Constructor name NoArguments)
    value = reflectSymbol (SProxy ∷ SProxy name)

  optionsParserImpl _ =
    Star (\s → if s == value
      then (pure ((Constructor NoArguments) ∷ Constructor name NoArguments))
      else (withExceptT Last $ ExceptT<<< pure $ Left (s)))
   where
    value = reflectSymbol (SProxy ∷ SProxy name)

toOptionValue ∷ ∀ opt optRep. (Generic opt optRep) ⇒ (Options optRep) ⇒ opt → String
toOptionValue v = toOptionValueImpl (from v)

options ∷ ∀ a aRep. (Generic a aRep) ⇒ (Options aRep) ⇒ Proxy a → List (Tuple String a)
options _ = map (to <$> _) (optionsImpl (Proxy ∷ Proxy aRep))

optionsParser ∷ ∀ a aRep e m. (Monad m) ⇒ (Generic a aRep) ⇒ (Options aRep) ⇒ Proxy a → FieldValidation m String String a
optionsParser _ = hoistStar (withExceptT unwrap) $ to <$> (optionsParserImpl (Proxy ∷ Proxy aRep))

class (Options c) ⇐ Choices c (c' ∷ # Type) | c → c' where
  -- validation result row
  -- choices rendering
  choicesParserImpl ∷ ∀ m. (Monad m) ⇒ (Proxy c) → FieldValidation m String (Array String) (Record c')

singletonRecord ∷ forall label row val
  . IsSymbol label
  ⇒ RowLacks label ()
  ⇒ RowCons label val () row
  ⇒  SProxy label
  → val
  → Record row
singletonRecord p v = insert p v {}

-- I've to witness a lot in order to compile this stuff ;-)
instance choicesConstructor ∷ (IsSymbol name, RowCons name Boolean () row, RowLacks name ()) ⇒ Choices (Constructor name NoArguments) row where
  choicesParserImpl proxy =
    parser
   where
    parser =
      let
        pName = (SProxy ∷ SProxy name)
      in
        Star (\vs → pure $ (singletonRecord (SProxy ∷ SProxy name) (any (reflectSymbol pName == _) vs)))

instance choicesSum ∷ (IsSymbol name, Choices b br, RowCons name Boolean br row, RowLacks name br) ⇒ Choices (Sum (Constructor name NoArguments) b) row where
  choicesParserImpl proxy =
    parser
   where
    parser =
      let
        pName = (SProxy ∷ SProxy name)
      in
        Star (\vs → do
          br ← unwrap (choicesParserImpl (Proxy ∷ Proxy b)) vs
          pure $ (insert (SProxy ∷ SProxy name) (any (reflectSymbol pName == _) vs) br))


choicesParser ∷ ∀ a aRep row m. (Monad m) ⇒ (Generic a aRep) ⇒ (Choices aRep row) ⇒ Proxy a → FieldValidation m String (Array String) (Record row)
choicesParser _ = (choicesParserImpl (Proxy ∷ Proxy aRep))

