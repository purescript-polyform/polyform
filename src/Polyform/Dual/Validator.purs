module Polyform.Dual.Validator where

import Polyform.Dual (Dual)
import Polyform.Validator (Validator) as Validator

type Validator m e i o = Dual (Validator.Validator m e) i o

