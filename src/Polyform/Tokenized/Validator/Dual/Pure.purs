module Polyform.Tokenized.Validator.Dual.Pure where

import Prelude

import Data.Identity (Identity(..))
import Data.List (List)
import Data.Newtype (un)
import Data.Validation.Semigroup (V)
import Polyform.Tokenized.Validator.Dual (Dual, DualD, runSerializer, runValidator) as Tokenized.Validator.Dual

type Dual err = Tokenized.Validator.Dual.Dual Identity err

type DualD err = Tokenized.Validator.Dual.DualD Identity err

runValidator ∷ ∀ err i o. Dual err i o → (List i → V err o)
runValidator = map (un Identity) <<< Tokenized.Validator.Dual.runValidator

runSerializer ∷ ∀ err i o. Dual err i o → (o → List i)
runSerializer = map (un Identity) <<< Tokenized.Validator.Dual.runSerializer

