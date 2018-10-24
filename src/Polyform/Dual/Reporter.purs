module Polyform.Dual.Reporter where

import Polyform.Dual (Dual)
import Polyform.Reporter (Reporter) as Reporter


type Reporter m e i o = Dual (Reporter.Reporter m e) i o
