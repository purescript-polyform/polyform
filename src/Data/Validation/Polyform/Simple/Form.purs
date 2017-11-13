module Data.Validation.Polyform.Simple.Form where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Data.Array (catMaybes)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Star (Star(..))
import Data.StrMap (StrMap, empty, fromFoldable, lookup)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(Tuple))
import Data.Validation.Polyform.Prim (V(..), Validation(..))
import Data.Variant (Variant, inj)
import Type.Prelude (class IsSymbol)

data Form field = Form (StrMap String) (Array field)
derive instance genericForm ∷ Generic (Form field) _
-- instance showForm ∷ (Show field) ⇒ Show (Form field) where show = genericShow

type ErrLabel = String
type ErrMessage = String
-- | Create form error in monadic context, so you can use translations/localizations etc.
formError ∷ ∀ a field m. (Apply m) ⇒ ErrLabel → m ErrMessage → m (V (Form field) a)
formError label = map (Invalid <<< (\msg → Form (fromFoldable [Tuple label msg]) []))

instance semigroupForm ∷ Semigroup (Form field) where
  append (Form e1 f1) (Form e2 f2)
    = Form (e1 <> e2) (f1 <> f2)

instance monoidForm ∷ Monoid (Form field) where
  mempty = Form empty mempty

type RawValue = String
data FieldValue a = FieldErr ErrMessage RawValue | FieldVal (Maybe a)
derive instance genericFieldValue ∷ Generic (FieldValue a) _
instance showFieldValue ∷ (Show a) ⇒ Show (FieldValue a) where show = genericShow

data Field
  = Input { label ∷ String, name ∷ String, value ∷ FieldValue String }
  | Password { label ∷ String, name ∷ String, value ∷ FieldValue String }
  | Number { label ∷ String, name ∷ String, value ∷ FieldValue Int }
--   -- | Radio { label ∷ String, name ∷ String, value ∷ Value Boolean }
--   -- | Select { label ∷ String, name ∷ String, options ∷ Array (Tuple String String), value ∷ FieldValue String}
--   -- | Checkbox { label ∷ String, name ∷ String, value ∷ Either (Tuple String Options) Options }
derive instance genericField ∷ Generic Field _
instance showField ∷ Show Field where show = genericShow

-- | HTTP query representation
type FieldQuery = Array (Maybe String)
type Query = StrMap FieldQuery

-- | Field validation - it is just sequential validation with `Either` as error carrier
-- | it seems sufficient for single field validation.
-- | Every FieldValidation combinator transforms `a` into `b`.
-- | Finally we want to have a ~ FieldQuery

type FieldValidation m e a b = Star (ExceptT e m) a b
type FieldValidation' m e b = FieldValidation m e FieldQuery b

-- | The same goes for whole form Validation
type FormValidation m b = Validation m (Form Field) Query b

tag :: forall a b e m p r r'
  . RowCons p e r r'
  ⇒ Monad m
  ⇒ IsSymbol p
  ⇒ SProxy p
  → FieldValidation m e a b
  → FieldValidation m (Variant r') a b
tag p v =
  wrap ((ExceptT <<< (lmap (inj p) <$> _) <<< runExceptT) <$> unwrap v)

scalar ∷ ∀ a m. (Monad m) ⇒ FieldValidation m (Array a) (Array a) a
scalar = Star $ \i → ExceptT <<< pure <<< s $ i
 where
  s [a] = Right a
  s arr = Left arr

scalar' ∷ ∀ a e m. (Monad m) ⇒ FieldValidation m (Variant (scalar ∷ Array a | e)) (Array a) a
scalar' = tag (SProxy ∷ SProxy "scalar") scalar

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

newtype Last a = Last a
derive instance newtypeLast ∷ Newtype (Last a) _
instance semigroupLast ∷ Semigroup (Last a) where
  append v1 v2 = v2

optional ∷ ∀ a b e err m. (Monad m)
  ⇒ FieldValidation m
      (Variant ( emptyOrMissing :: Array (Maybe String) | _))
      FieldQuery
      b
  → FieldValidation m _ FieldQuery (Maybe b)
optional v =
  optV
 where
  empty' =
    let Star y = const Nothing <$> emptyOrMissing'
    in withExceptT Last <$> y
  v' =
    let Star x = Just <$> v
    in withExceptT Last <$> x
  optV =
    let Star o = Star empty' <|> Star v'
    in Star (withExceptT unwrap <$> o)

int ∷ ∀ m. (Monad m) ⇒ FieldValidation m String String Int
int = Star $ \s → ExceptT <<< pure $ note s (fromString s)

int' ∷ ∀ m e. (Monad m) ⇒ FieldValidation m (Variant (int ∷ String | e)) String Int
int' = tag (SProxy ∷ SProxy "int") int

catMaybesV :: forall a e m. (Monad m) ⇒ FieldValidation m e (Array (Maybe a)) (Array a)
catMaybesV = Star $ ExceptT <<< pure <<< Right <<< catMaybes

nonEmptyScalar' ∷ ∀ m v. (Monad m) ⇒ FieldValidation m (Variant (nonEmptyScalar ∷ Unit, scalar ∷ Array String | v)) (Array (Maybe String)) String
nonEmptyScalar' =
  catMaybesV >>> scalar' >>> (tag (SProxy ∷ SProxy "nonEmptyScalar") (Star $ ExceptT <<< pure <<< (\s →
    if s == mempty
      then Left unit
      else Right s)))

check ∷ ∀ a e field m. (Monad m) ⇒ String → m String → (a → m Boolean) → Validation m (Form field) a a
check errLabel mErrMsg c =
  Validation (\a → do
    r ← c a
    if r
      then pure (pure a)
      else formError errLabel mErrMsg)

type Form' = Form Field

field ∷ ∀ a e i m
  . (Monad m)
  ⇒ String
  → m String
  → ({ label ∷ String, name ∷ String, value ∷ FieldValue a }  → Field)
  → FieldValidation' m e a
  → FormValidation m a
field name l c validator = Validation $ \q → do
  let i = fromMaybe [] (lookup name q)
  r ← runExceptT (unwrap validator i)
  label ← l
  pure $ case r of
    Left e → Invalid (Form empty [c { label, name, value: FieldErr "Appropriate error message..." "" }])
    Right v → Valid (Form empty [c { label, name, value: FieldVal (Just v) }]) v

fieldOpt ∷ ∀ a e i m
  . Monad m
  ⇒ String
  → m String
  → ({ label ∷ String, name ∷ String, value ∷ FieldValue a } → Field)
  → FieldValidation' m e (Maybe a)
  → FormValidation m (Maybe a)
fieldOpt name mLabel constructor validator = Validation $ \q → do
  let i = fromMaybe [] (lookup name q)
  r ← runExceptT (unwrap validator i)
  label ← mLabel
  pure $ case r of
    Left e → Invalid (Form empty [ (constructor { label, name, value: FieldErr "Appropriate error message..." "" }) ])
    Right v → Valid (Form empty [ (constructor { label, name, value: FieldVal v }) ]) v

input name mLabel = field name mLabel Input nonEmptyScalar'

inputOpt ∷ forall t665. Monad t665 => String -> t665 String -> Validation t665 (Form Field) (StrMap (Array (Maybe String))) (Maybe String)
inputOpt name mLabel = fieldOpt name mLabel Input (optional nonEmptyScalar')

password name mLabel = field name mLabel Password nonEmptyScalar'

passwordOpt ∷ forall t665. Monad t665 => String -> t665 String -> Validation t665 (Form Field) (StrMap (Array (Maybe String))) (Maybe String)
passwordOpt name mLabel = fieldOpt name mLabel Password (optional nonEmptyScalar')

number name mLabel = field name mLabel Number (nonEmptyScalar' >>> int')

numberOpt ∷ forall t665. Monad t665 => String -> t665 String -> Validation t665 (Form Field) (StrMap (Array (Maybe String))) (Maybe Int)
numberOpt name mLabel = fieldOpt name mLabel Number (optional (nonEmptyScalar' >>> int'))

