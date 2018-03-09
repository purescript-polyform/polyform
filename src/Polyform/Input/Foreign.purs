module Polyform.Input.Foreign where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (mapWithIndex, singleton)
import Data.Array as Array
import Data.Bifunctor as Bifunctor
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Foreign (Foreign, ForeignError(..), MultipleErrors, readArray, readInt, readString)
import Data.Foreign.Index (class Index, (!))
import Data.Functor.Variant (VariantF)
import Data.Monoid (class Monoid)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Polyform.Field as Field
import Polyform.Validation (V(..), Validation(..), fromEither, hoistFnMV, hoistFnV, lmapValidation, runValidation)
import Type.Prelude (class IsSymbol, SProxy(..))

type FieldErr = Variant (value ∷ MultipleErrors)

type Field attrs err value = { value :: V err value | attrs }

type IntField attrs = Field attrs FieldErr Int
type StringField attrs = Field attrs FieldErr String
type NumberField attrs = Field attrs FieldErr Number
-- | Val is a self reference to an object
type ArrayField attrs e val = Field attrs (Array e) (Array val)
-- | This probably seems strange but we are not able to handle
-- | real value on our monoidal representation level.
-- | Don't worry - real, valid and fully typed values are going
-- | to be aggregated on the validation level ;-)
type ObjectField val = { value ∷ Array (Attr val) }

data Attr a = Attr String (Variant (index ∷ MultipleErrors, value ∷ a))

-- | THIS IS STILL PLAYGROUND
-- | We don't really want this fields and
-- | they should be proviede by the user.
data MyField
  = IntField (IntField ())
  | StringField (StringField ())
  | NumberField (NumberField ())
  | Object (ObjectField MyField)

attr
  ∷ ∀ m v
  . Monad m
  ⇒ String
  → Validation m MyField Foreign v
  → Validation m (Array (Attr MyField)) Foreign v
attr name v = hoistFnMV \input → do
  let r = runExcept (input ! name)
  case r of
    Left e → pure $ Invalid (singleton $ Attr name (inj (SProxy ∷ SProxy "index") e))
    Right input' → do
      let v' = lmapValidation (\e → singleton $ Attr name (inj (SProxy ∷ SProxy "value") e)) v
      runValidation v' input'

object
  ∷ ∀ m v
  . Monad m
  ⇒ Validation m (Array (Attr MyField)) Foreign v
  → Validation m MyField Foreign v
object = lmapValidation (Object <<< { value: _ })

arrayFieldsValidation
  ∷ ∀ e es m v m
  . Monad m
  ⇒ Validation m e Foreign v
  → Validation m (Array e) (Array Foreign) (Array v)
arrayFieldsValidation v = hoistFnMV $ \arr → do
  arr'← sequence $ mapWithIndex validateItem arr
  pure $ fold arr'
 where
  validateItem index item =
    Bifunctor.bimap Array.singleton Array.singleton <$> runValidation v item

arrayValidation v =
  arr >>> fields
 where
  fields =
    lmapValidation
      (singleton <<< inj (SProxy ∷ SProxy "values"))
      (arrayFieldsValidation v)

  arr = hoistFnV $ readArray >>> runExcept >>= \eitherArr → do
    let arr' = Bifunctor.lmap (singleton <<< inj (SProxy ∷ SProxy "array")) eitherArr
    pure (fromEither $ arr')

intValidation
  ∷ ∀ m err
  . Monad m
  ⇒ Validation m (Array (Variant (value ∷ MultipleErrors | err))) Foreign Int
intValidation =
  hoistFnV (readInt >>> runExcept >>> (fromEither <<< (Bifunctor.lmap (singleton <<< inj (SProxy ∷ SProxy "value")))))

stringValidation
  ∷ ∀ m err
  . Monad m
  ⇒ Validation m (Array (Variant (value ∷ MultipleErrors | err))) Foreign String
stringValidation =
  hoistFnV (readString >>> runExcept >>> (fromEither <<< (Bifunctor.lmap (singleton <<< inj (SProxy ∷ SProxy "value")))))

