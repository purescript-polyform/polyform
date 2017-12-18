module Test.Main where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.Tuple.Nested ((/\))
import Data.Validation.Polyform.Field (choicesParser, options, optionsParser)
import Data.Validation.Polyform.Field.Option (type (:-), Nil)
import Data.Validation.Polyform.Field.Option as Option
import Data.Validation.Polyform.Validation.Field (runFieldValidation)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Output.Fancy (runTest)
import Type.Prelude (Proxy(Proxy))

data Opts = X | Y | Z
derive instance genericOpts ∷ Generic Opts _
derive instance eqOpts ∷ Eq Opts
instance showOpts ∷ Show Opts where
  show = genericShow

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
main = launchAff $ runTest $ do
  suite "Options type class for coproduct type" do
    test "all options are generated" $ do
      let opts = options (Proxy ∷ Proxy Opts)
      equal (("X" /\ X) : ("Y" /\ Y) : ("Z" /\ Z) : Nil) opts
    let prs = optionsParser (Proxy ∷ Proxy Opts)
    test "parser parses constructors" $ do
      equal (runExceptT (runFieldValidation prs "X")) (Identity $ Right X)
      equal (runExceptT (runFieldValidation prs "Y")) (Identity $ Right Y)
      equal (runExceptT (runFieldValidation prs "Z")) (Identity $ Right Z)
    test "parser fails for constructor prefix" $ do
      equal (runExceptT (runFieldValidation prs "X'")) (Identity $ Left ("X'"))
    test "parser fails for wrong value" $ do
      equal (runExceptT (runFieldValidation prs "Unkown")) (Identity $ Left ("Unkown"))
  suite "choicesParser" $ do
    suite "for coproduct type" $ do
      let prs = choicesParser (Proxy ∷ Proxy Opts)
      test "parses single value" $ do
        equal (runExceptT (runFieldValidation ((_."X") <$> prs) ["X"])) (Identity $ Right true)
        equal (runExceptT (runFieldValidation ((_."Y") <$> prs) ["X"])) (Identity $ Right false)
        equal (runExceptT (runFieldValidation ((_."Z") <$> prs) ["X"])) (Identity $ Right false)
      test "parses multiple values" $ do
        equal (runExceptT (runFieldValidation ((_."X") <$> prs) ["X", "Z"])) (Identity $ Right true)
        equal (runExceptT (runFieldValidation ((_."Y") <$> prs) ["X", "Z"])) (Identity $ Right false)
        equal (runExceptT (runFieldValidation ((_."Z") <$> prs) ["X", "Z"])) (Identity $ Right true)
      test "parses no values" $ do
        equal (runExceptT (runFieldValidation ((_."X") <$> prs) [])) (Identity $ Right false)
        equal (runExceptT (runFieldValidation ((_."Y") <$> prs) [])) (Identity $ Right false)
        equal (runExceptT (runFieldValidation ((_."Z") <$> prs) [])) (Identity $ Right false)
      test "parses repeated value" $ do
        equal (runExceptT (runFieldValidation ((_."X") <$> prs) ["Y", "Y", "X"])) (Identity $ Right true)
        equal (runExceptT (runFieldValidation ((_."Y") <$> prs) ["Y", "Y", "X"])) (Identity $ Right true)
        equal (runExceptT (runFieldValidation ((_."Z") <$> prs) ["Y", "Y", "X"])) (Identity $ Right false)
    suite "for Symbol list" $ do
      let prs = Option.choicesParser (Proxy ∷ Proxy ("X" :- "Y" :- "Z" :- Nil))
      test "parses single value" $ do
        equal (runExceptT (runFieldValidation ((_."X") <$> prs) ["X"])) (Identity $ Right true)
        equal (runExceptT (runFieldValidation ((_."Y") <$> prs) ["X"])) (Identity $ Right false)
        equal (runExceptT (runFieldValidation ((_."Z") <$> prs) ["X"])) (Identity $ Right false)
      test "parses multiple values" $ do
        equal (runExceptT (runFieldValidation ((_."X") <$> prs) ["X", "Z"])) (Identity $ Right true)
        equal (runExceptT (runFieldValidation ((_."Y") <$> prs) ["X", "Z"])) (Identity $ Right false)
        equal (runExceptT (runFieldValidation ((_."Z") <$> prs) ["X", "Z"])) (Identity $ Right true)
      test "parses no values" $ do
        equal (runExceptT (runFieldValidation ((_."X") <$> prs) [])) (Identity $ Right false)
        equal (runExceptT (runFieldValidation ((_."Y") <$> prs) [])) (Identity $ Right false)
        equal (runExceptT (runFieldValidation ((_."Z") <$> prs) [])) (Identity $ Right false)
      test "parses repeated value" $ do
        equal (runExceptT (runFieldValidation ((_."X") <$> prs) ["Y", "Y", "X"])) (Identity $ Right true)
        equal (runExceptT (runFieldValidation ((_."Y") <$> prs) ["Y", "Y", "X"])) (Identity $ Right true)
        equal (runExceptT (runFieldValidation ((_."Z") <$> prs) ["Y", "Y", "X"])) (Identity $ Right false)
