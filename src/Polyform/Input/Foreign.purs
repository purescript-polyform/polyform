module Polyform.Input.Foreign where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (mapWithIndex, singleton)
import Data.Array as Array
import Data.Bifunctor as Bifunctor
import Data.Either (Either(..))
import Data.Foldable (fold)
import Foreign (Foreign, MultipleErrors, readArray, readInt, readString)
import Foreign.Index ((!))
import Data.Traversable (sequence)
import Data.Variant (Variant, inj)
import Polyform.Validation (V(Valid, Invalid), Validation, fromEither, hoistFnMV, hoistFnV, lmapValidation, runValidation)
import Type.Prelude (SProxy(SProxy))

-- | WARNING!
-- | It is still prototyping phase
-- | of this module.
type FieldErr = Variant (value ∷ MultipleErrors)

type Field attrs err value = { value ∷ V err value | attrs }

type IntField attrs = Field attrs FieldErr Int
type StringField attrs = Field attrs FieldErr String
type NumberField attrs = Field attrs FieldErr Number
-- | Val is a self reference to an object
type ArrayField attrs e val = Field attrs (Array e) (Array val)

-- | You should wrap this array into
-- | your constructors which represent
-- | some type of object.
type Attrs val = Array (Attr val)

-- | `Attr` wraps other field and attaches attribute name to it
data AttrErr field
  = AttrIndexErr Foreign MultipleErrors
  | AttrFieldErr Foreign field
data Attr field = Attr
  { name ∷ String
  , value ∷ Either (AttrErr field) field
  }

-- | You should define your fields using something
-- | like:
-- |
-- | data MyField
-- |   = IntField (IntField ())
-- |   | StringField (StringField ())
-- |   | NumberField (NumberField ())
-- |   | Object (Attrs MyField)
-- |
-- | What is really imporant is that `Attrs` wrapper should be recursive.

attr
  ∷ ∀ e field m v
  . Monad m
  ⇒ (e → field)
  → String
  → Validation m e Foreign v
  → Validation m (Array (Attr field)) Foreign v
attr constructor name v = hoistFnMV \input → do
  let
    r = runExcept (input ! name)
    attr' value = singleton $ Attr { name, value }
    invalid = Invalid <<< attr' <<< Left
  case r of
    Left e →
      pure $ invalid $ AttrIndexErr input e
    Right input' → do
      r' ← runValidation v input'
      pure $ case r' of
        Invalid e' →
          invalid (AttrFieldErr input' (constructor e'))
        Valid e result →
          Valid (attr' (Right (constructor e))) result

object
  ∷ ∀ field m v
  . Monad m
  ⇒ (Array (Attr field) → field)
  → Validation m (Array (Attr field)) Foreign v
  → Validation m field Foreign v
object constructor = lmapValidation constructor

arrayFieldsValidation
  ∷ ∀ e m v
  . Monad m
  ⇒ Validation m e Foreign v
  → Validation m (Array e) (Array Foreign) (Array v)
arrayFieldsValidation v = hoistFnMV $ \arr → do
  arr'← sequence $ mapWithIndex validateItem arr
  pure $ fold arr'
 where
  validateItem index item =
    Bifunctor.bimap Array.singleton Array.singleton <$> runValidation v item

arrayValidation :: forall t191 t194 t198 t236.
   Monad t198 => Validation t198 t194 Foreign t191
                 -> Validation t198
                      (Array
                         (Variant
                            ( values :: Array t194
                            , array :: MultipleErrors
                            | t236
                            )
                         )
                      )
                      Foreign
                      (Array t191)
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

