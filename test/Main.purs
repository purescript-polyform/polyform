module Test.Main where

import Prelude

import Effect (Effect)
import Test.Polyform.Reporter (suite) as Reporter
import Test.Polyform.Validator (suite) as Validator

main :: Effect Unit
main = do
  Validator.suite
  Reporter.suite
