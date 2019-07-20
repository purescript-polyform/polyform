module Polyform.Duals.Validator where

import Prelude

import Data.Validation.Semigroup (V)
import Polyform.Dual (Dual, DualD, parser, serializer) as Dual
import Polyform.Validator (Validator)
import Polyform.Validator as Validator
import Polyform.Validator.Par as Validator.Par

type Dual m e i o = Dual.Dual (Validator m e) i o

type DualD m e i o' o = Dual.DualD (Validator m e) i o o'

runValidator ∷ ∀ e i o m. Monad m ⇒ Dual.Dual (Validator m e) i o → (i → m (V e o))
runValidator = Validator.runValidator <<< Dual.parser

-- | Dirty hack to simplify type inference for polymorphic duals
runSerializer ∷ ∀ e i o m. Applicative m ⇒ Dual.Dual (Validator m e) i o → (o → i)
runSerializer = Dual.serializer

runSerializerM ∷ ∀ e i o m. Applicative m ⇒ Dual.Dual (Validator m e) i o → (o → m i)
runSerializerM = map pure <<< Dual.serializer

newtype Par m r a b = Dual (Validator.Par.Par m r a b)
