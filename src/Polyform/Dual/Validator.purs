module Polyform.Dual.Validator where

import Prelude

import Data.Newtype (unwrap)
import Data.Validation.Semigroup (V)
import Polyform.Dual (Dual)
import Polyform.Validator as Validator
import Polyform.Validator.Par as Validator.Par

type Validator m e i o = Dual (Validator.Validator m e) i o

runValidator ∷ ∀ e i o m. Monad m ⇒ Dual (Validator.Validator m e) i o → (i → m (V e o))
runValidator = Validator.runValidator <<< _.parser <<< unwrap <<< unwrap

runSerializer ∷ ∀ e i o m. Dual (Validator.Validator m e) i o → (o → i)
runSerializer = _.serializer <<< unwrap <<< unwrap

-- | Dirty hack to simplify type inference for polymorphic duals
runSerializer' ∷ ∀ e i o m. Applicative m ⇒ Dual (Validator.Validator m e) i o → (o → m i)
runSerializer' = map pure <<< _.serializer <<< unwrap <<< unwrap

newtype Par m r a b = Dual (Validator.Par.Par m r a b)
