module Test.Main where

import Prelude
import Test.Polyform.Field.Generic as Generic
import Test.Polyform.Input.Http as Http
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Console (CONSOLE)
import Test.Unit (suite)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall eff. Eff ( timer :: TIMER
                        , avar :: AVAR
                        , console :: CONSOLE
                        , testOutput :: TESTOUTPUT | eff ) Unit
main = runTest $ do
  suite "Polyform.Field.Generic" Generic.suite
  suite "Polyform.Input.Http" Http.suite
