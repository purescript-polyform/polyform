module Polyform.Dual.Reporter where

import Polyform.Dual (Dual)
import Polyform.Reporter (Reporter) as Reporter
import Polyform.Reporter.Par as Reporter.Par


type Reporter m e i o = Dual (Reporter.Reporter m e) i o

newtype Par m r a b = Dual (Reporter.Par.Par m r a b)
