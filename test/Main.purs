module Test.Main where

import Prelude
import Test.Polyform.Field.Generic as Generic
import Test.Polyform.Input.Http as Http
import Effect (Effect)
import Test.Unit (suite)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest $ do
  suite "Polyform.Field.Generic" Generic.suite
  suite "Polyform.Input.Http" Http.suite
