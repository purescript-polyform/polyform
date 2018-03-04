module Polyform.Input.Foreign where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor as Bifunctor
import Data.Foreign (Foreign, MultipleErrors, readInt, readString)
import Data.Foreign.Index (class Index, (!))
import Data.Variant (Variant, inj)
import Polyform.Field as Field
import Polyform.Form.Component as Form.Component
import Type.Prelude (SProxy(..))

type ForeignErr err = (indexErrors ∷ MultipleErrors, valueErrors ∷ MultipleErrors | err)

type Field attrs index err value =
  Field.Input attrs index (Variant (ForeignErr err)) value

type IntField attrs index err = Field attrs index err Int
type StringField attrs index err = Field attrs index err String
type NumberField attrs index err = Field attrs index err Number

intValidation
  ∷ ∀ m err
  . Monad m
  ⇒ Field.Validation m (Variant (valueErrors ∷ MultipleErrors | err)) Foreign Int
intValidation =
  Field.hoistEither (readInt >>> runExcept >>> Bifunctor.lmap (inj (SProxy ∷ SProxy "valueErrors")))

stringValidation
  ∷ ∀ m err
  . Monad m
  ⇒ Field.Validation m (Variant (valueErrors ∷ MultipleErrors | err)) Foreign String
stringValidation =
  Field.hoistEither (readString >>> runExcept >>> Bifunctor.lmap (inj (SProxy ∷ SProxy "valueErrors")))

fromFieldCoerce
  ∷ ∀ attrs err form index m value value'
  . Index index
  ⇒ Monad m
  ⇒ (value → value')
  → (Field attrs index err value -> form)
  → (Field attrs index err value)
  → Field.Validation m (Variant (ForeignErr err)) Foreign value
  → Form.Component.Component m form Foreign value'
fromFieldCoerce coerce singleton field validation =
  Form.Component.fromFieldCoerce coerce singleton field (index >>> validation)
 where
  index = Field.hoistEither $ \v →
    Bifunctor.lmap (inj (SProxy ∷ SProxy "indexErrors")) (runExcept (v ! field.name))

fromField
  ∷ ∀ attrs err form index m value
  . Index index
  ⇒ Monad m
  ⇒ (Field attrs index err value -> form)
  → (Field attrs index err value)
  → Field.Validation m (Variant (ForeignErr err)) Foreign value
  → Form.Component.Component m form Foreign value
fromField = fromFieldCoerce id
