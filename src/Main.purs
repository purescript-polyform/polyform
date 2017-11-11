module Main where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Array (catMaybes)
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Star (Star(..))
import Data.StrMap (StrMap, fromFoldable, lookup)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (Variant, inj)
import Debug.Trace (traceAnyA)
import Type.Prelude (class IsSymbol, reflectSymbol)

data V e a = Invalid e | Valid e a
derive instance functorV ∷ Functor (V e)

instance bifunctorV ∷ Bifunctor V where
  bimap f _ (Invalid e) = Invalid (f e)
  bimap f g (Valid e a) = Valid (f e) (g a)

instance altV ∷ Alt (V e) where
  alt (Invalid _) v = v
  alt v _ = v

valid ∷ ∀ a e. (Monoid e) ⇒ a → V e a
valid a = Valid mempty a

newtype Validation m e a b = Validation (a → m (V e b))
derive instance newtypeVaildation ∷ Newtype (Validation m e a b) _
derive instance functorValidation ∷ (Functor m) ⇒ Functor (Validation m e a)

runValidation (Validation f) = f

pureV ∷ ∀ a b e m. (Monad m) ⇒ (Monoid e) ⇒ (a → b) →  Validation m e a b
pureV f = Validation $ pure <<< valid <<< f

instance applyValidation ∷ (Semigroup e, Monad m) ⇒ Apply (Validation m e a) where
  apply vf va = Validation $ \i → do
    vf' ← unwrap vf i
    va' ← unwrap va i
    pure $ case vf', va' of
      Valid m1 f, Valid m2 a → Valid (m1 <> m2) (f a)
      Invalid m1, Valid m2 _ → Invalid (m1 <> m2)
      Invalid m1, Invalid m2 → Invalid (m1 <> m2)
      Valid m1 _, Invalid m2 → Invalid (m1 <> m2)

instance semigroupoidValidation ∷ (Monad m, Semigroup e) ⇒ Semigroupoid (Validation m e) where
  compose v2 v1 =
    Validation $ (\a → do
      eb ← unwrap v1 a
      case eb of
        Valid e b → do
          r ← unwrap v2 b
          pure $ case r of
            Valid e' c → Valid (e <> e') c
            Invalid e' → Invalid (e <> e')
        Invalid e → pure (Invalid e))


data Form = Form (Array String) (Array Field)
derive instance genericForm ∷ Generic Form _
instance showForm ∷ Show Form where show = genericShow

formError ∷ ∀ a. String → V Form a
formError msg = Invalid $ Form [msg] []

instance semigroupForm ∷ Semigroup Form where
  append (Form e1 f1) (Form e2 f2)
    = Form (e1 <> e2) (f1 <> f2)

instance monoidForm ∷ Monoid Form where
  mempty = Form [] []

-- | Move to this representation
data FieldValue a = Err String String | Val (Maybe a)
derive instance genericFieldValue ∷ Generic (FieldValue a) _
instance showFieldValue ∷ (Show a) ⇒ Show (FieldValue a) where show = genericShow

type Value = String
type Checked = Boolean
type Label = String
newtype Option = Option
  { value ∷ String
  , checked ∷ Boolean
  , label ∷ String
  }
derive instance newtypeOption ∷ Newtype Option _
derive instance genericOption ∷ Generic Option _
instance showOption ∷ Show Option where
  show = genericShow
type Options = Array Option

option :: String -> Boolean -> String -> Option
option v c l = Option { value: v, checked: c, label: l }

data Field
  = Input { label ∷ String, name ∷ String, value ∷ FieldValue String }
  | Password { label ∷ String, name ∷ String, value ∷ FieldValue String }
  | Number { label ∷ String, name ∷ String, value ∷ FieldValue Int }
  -- | Radio { label ∷ String, name ∷ String, value ∷ Value Boolean }
  -- | Select { label ∷ String, name ∷ String, options ∷ Array (Tuple String String), value ∷ FieldValue String}
  -- | Checkbox { label ∷ String, name ∷ String, value ∷ Either (Tuple String Options) Options }
derive instance genericField ∷ Generic Field _
instance showField ∷ Show Field where show = genericShow

-- | http query representation
type Query = StrMap (Array (Maybe String))
type QueryField = Array (Maybe String)

-- | Field validators combinators

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

scalar ∷ ∀ a m. (Monad m) ⇒ FieldValidation m (Array a) (Array a) a
scalar = Star $ \i → ExceptT <<< pure <<< s $ i
 where
  s [a] = Right a
  s arr = Left arr

missing ∷ ∀ a m. (Monad m) ⇒ FieldValidation m QueryField QueryField Unit
missing = Star $ \i → ExceptT <<< pure <<< s $ i
 where
  s [] = Right unit
  s arr = Left arr

optional ∷ ∀ a b e m. (Monad m) ⇒ (Semigroup e) ⇒ FieldValidation m QueryField QueryField b → FieldValidation m QueryField QueryField (Maybe b)
optional v = (const Nothing <$> missing) <|> (Just <$> v)

scalar' ∷ ∀ a e m. (Monad m) ⇒ FieldValidation m (Variant (scalar ∷ Array a | e)) (Array a) a
scalar' = tag (SProxy ∷ SProxy "scalar") scalar

int ∷ ∀ m. (Monad m) ⇒ FieldValidation m String String Int
int = Star $ \s → ExceptT <<< pure $ note s (fromString s)

int' ∷ ∀ m e. (Monad m) ⇒ FieldValidation m (Variant (int ∷ String | e)) String Int
int' = tag (SProxy ∷ SProxy "int") int

catMaybesV :: forall a e m. (Monad m) ⇒ FieldValidation m e (Array (Maybe a)) (Array a)
catMaybesV = Star $ ExceptT <<< pure <<< Right <<< catMaybes

nonEmptyString' ∷ ∀ m v. (Monad m) ⇒ FieldValidation m (Variant (nonEmptyString ∷ Unit, scalar ∷ Array String | v)) (Array (Maybe String)) String
nonEmptyString' =
  catMaybesV >>> scalar' >>> (tag (SProxy ∷ SProxy "nonEmptyString") (Star $ ExceptT <<< pure <<< (case _ of
    "" → Left unit
    s → Right s)))

_input ∷ ∀ m
  . Monad m
  ⇒ _
  → String
  → String
  → Validation m Form Query String
_input c name label =
  Validation v
 where
   v = \q → do
    let i = fromMaybe [] (lookup name q)
    r ← runExceptT (unwrap nonEmptyString' i)
    pure $ case r of
      Left e → (Invalid (Form [] [c { label, name, value: Err "Appropriate error message..." "" }]))
      Right v → (Valid (Form [] [c { label, name, value: Val (Just v) }]) v)

input = _input Input
password = _input Password

field ∷ ∀ a e i m. (Monad m) ⇒ String → String → ({ label ∷ String, name ∷ String, value ∷ FieldValue a }  → Field) → FieldValidation m e i a → (Validation m Field i a)
field name label c val = Validation $ \i → do
  r ← runExceptT (unwrap val i)
  pure $ case r of
    Left e → (Invalid (c { label, name, value: Err "Appropriate error message..." "" }))
    Right v → (Valid (c { label, name, value: Val (Just v) }) v)

-- fieldOpt ∷ ∀ a e i m. (Monad m) ⇒ String → String → ({ label ∷ String, name ∷ String, value ∷ FieldValue a }  → Field) → FieldValidation m e i (Maybe a) → (i → m (V Field (Maybe a)))
-- fieldOpt name label c val = \i → do
--   r ← runExceptT (unwrap val i)
--   pure $ case r of
--     Left e → (Invalid (c { label, name, value: Err "Appropriate error message..." "" }))
--     Right v → (Valid (c { label, name, value: Val v }) v)


number ∷ ∀ m
  . Monad m
  ⇒ String
  → String
  → Validation m Form Query Int
number name label =
  Validation v
 where
   v = \q → do
    r ← runExceptT (unwrap (nonEmptyString' >>> int') (fromMaybe [] (lookup name q)))
    pure $ case r of
      Left e → (Invalid (Form [] [Number { label, name, value: Err "Appropriate error message..." "" }]))
      Right v → (Valid (Form [] [Number { label, name, value: Val (Just v) }]) v)

check ∷ ∀ a m. (Monad m) ⇒ String → (a → Boolean) → Validation m Form a a
check msg c =
  Validation (\a → pure $ if c a
    then valid a
    else formError msg)


newtype Profile = Profile
  { nickname ∷ String
  , bio ∷ String
  , age ∷ Int
  , password ∷ String
  }
derive instance genericProfile ∷ Generic Profile _
instance showProfile ∷ Show Profile where
  show = genericShow

passwordV = (Tuple <$> password "password1" "Password" <*> password "password2" "Password (repeat)") >>> check "Password do not match" (\p → fst p == snd p) >>> pureV (\p → fst p)

profile :: forall m.
   Monad m => Validation m Form Query Profile
profile = Profile <$> ({nickname: _, bio: _, age: _, password: _} <$> input "nickname" "Nickname" <*> input "bio" "Bio" <*> number "age" "Age" <*> passwordV)

validateAndPrint ∷ ∀ a. (Show a) ⇒ Validation _ Form _ a → _ → _
validateAndPrint v input = do
  f ← runValidation v input
  log "\n"
  case f of
    Valid f v → do
      logShow v
      logShow f
    Invalid f → logShow f


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  validateAndPrint passwordV (fromFoldable [Tuple "password1" [Just "admin"]])
  -- (Form [] [(Input { label: "Password", name: "password1", value: (Val (Just "admin")) }),(Input { label: "Password (repeat)", name: "password2", value: (Err "Appropriate error message..." "") })])

  validateAndPrint passwordV (fromFoldable [Tuple "password1" [Just "admin"], Tuple "password2" [Just "pass"]])
  -- (Form ["Password do not match"] [(Input { label: "Password", name: "password1", value: (Val (Just "admin")) }),(Input { label: "Password (repeat)", name: "password2", value: (Val (Just "pass")) })])

  validateAndPrint passwordV (fromFoldable [Tuple "password1" [Just "secret"], Tuple "password2" [Just "secret"]])
  -- (Form [] [(Input { label: "Password", name: "password1", value: (Val (Just "secret")) }),(Input { label: "Password (repeat)", name: "password2", value: (Val (Just "secret")) })])


  let
    onlyNickname =
      (fromFoldable
        [Tuple "nickname" [Just "nick"]])
  validateAndPrint profile onlyNickname

  let
    nicknameAndPassword =
      (fromFoldable
        [ Tuple "nickname" [Just "nick"]
        , Tuple "password1" [Just "new"]
        , Tuple "password2" [Just "new"]])

  validateAndPrint profile nicknameAndPassword

  let
    nicknameAndPasswordMismatch =
      (fromFoldable
        [ Tuple "nickname" [Just "nick"]
        , Tuple "password1" [Just "wrong"]
        , Tuple "password2" [Just "new"]])

  validateAndPrint profile nicknameAndPasswordMismatch

  let
    fullProfile =
      (fromFoldable
        [ Tuple "nickname" [Just "nick"]
        , Tuple "bio" [Just "bio"]
        , Tuple "age" [Just "666"]
        , Tuple "password1" [Just "new"]
        , Tuple "password2" [Just "new"]])

  validateAndPrint profile fullProfile
