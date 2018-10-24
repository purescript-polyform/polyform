module Polyform.Dual.Validator where

import Polyform.Dual (Dual)
import Polyform.Validator as Validator
import Polyform.Validator.Par as Validator.Par

type Validator m e i o = Dual (Validator.Validator m e) i o

newtype Par m r a b = Dual (Validator.Par.Par m r a b)
