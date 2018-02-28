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
import Polyform.Field (runValidation)
import Polyform.Field.Generic (choiceParser, choices, multiChoiceParser)
import Polyform.Field.Generic.Option as Option
import Polyform.Field.Generic.Option (type (:-), Nil)
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
      let opts = choices (Proxy ∷ Proxy Opts)
      equal (("X" /\ X) : ("Y" /\ Y) : ("Z" /\ Z) : Nil) opts
    let prs = choiceParser (Proxy ∷ Proxy Opts)
    test "parser parses constructors" $ do
      equal (runExceptT (runValidation prs "X")) (Identity $ Right X)
      equal (runExceptT (runValidation prs "Y")) (Identity $ Right Y)
      equal (runExceptT (runValidation prs "Z")) (Identity $ Right Z)
    test "parser fails for constructor prefix" $ do
      equal (runExceptT (runValidation prs "X'")) (Identity $ Left ("X'"))
    test "parser fails for wrong value" $ do
      equal (runExceptT (runValidation prs "Unkown")) (Identity $ Left ("Unkown"))
  suite "multiChoiceParser" $ do
    suite "for coproduct type" $ do
      let prs = _.product <$> multiChoiceParser (Proxy ∷ Proxy Opts)
      test "parses single value" $ do
        equal (runExceptT (runValidation ((_."X") <$> prs) ["X"])) (Identity $ Right true)
        equal (runExceptT (runValidation ((_."Y") <$> prs) ["X"])) (Identity $ Right false)
        equal (runExceptT (runValidation ((_."Z") <$> prs) ["X"])) (Identity $ Right false)
      test "parses multiple values" $ do
        equal (runExceptT (runValidation ((_."X") <$> prs) ["X", "Z"])) (Identity $ Right true)
        equal (runExceptT (runValidation ((_."Y") <$> prs) ["X", "Z"])) (Identity $ Right false)
        equal (runExceptT (runValidation ((_."Z") <$> prs) ["X", "Z"])) (Identity $ Right true)
      test "parses no values" $ do
        equal (runExceptT (runValidation ((_."X") <$> prs) [])) (Identity $ Right false)
        equal (runExceptT (runValidation ((_."Y") <$> prs) [])) (Identity $ Right false)
        equal (runExceptT (runValidation ((_."Z") <$> prs) [])) (Identity $ Right false)
      test "parses repeated value" $ do
        equal (runExceptT (runValidation ((_."X") <$> prs) ["Y", "Y", "X"])) (Identity $ Right true)
        equal (runExceptT (runValidation ((_."Y") <$> prs) ["Y", "Y", "X"])) (Identity $ Right true)
        equal (runExceptT (runValidation ((_."Z") <$> prs) ["Y", "Y", "X"])) (Identity $ Right false)
    suite "for Symbol list" $ do
      let prs = _.product <$> Option.multiChoiceParser (Proxy ∷ Proxy ("X" :- "Y" :- "Z" :- Nil))
      test "parses single value" $ do
        equal (runExceptT (runValidation ((_."X") <$> prs) ["X"])) (Identity $ Right true)
        equal (runExceptT (runValidation ((_."Y") <$> prs) ["X"])) (Identity $ Right false)
        equal (runExceptT (runValidation ((_."Z") <$> prs) ["X"])) (Identity $ Right false)
      test "parses multiple values" $ do
        equal (runExceptT (runValidation ((_."X") <$> prs) ["X", "Z"])) (Identity $ Right true)
        equal (runExceptT (runValidation ((_."Y") <$> prs) ["X", "Z"])) (Identity $ Right false)
        equal (runExceptT (runValidation ((_."Z") <$> prs) ["X", "Z"])) (Identity $ Right true)
      test "parses no values" $ do
        equal (runExceptT (runValidation ((_."X") <$> prs) [])) (Identity $ Right false)
        equal (runExceptT (runValidation ((_."Y") <$> prs) [])) (Identity $ Right false)
        equal (runExceptT (runValidation ((_."Z") <$> prs) [])) (Identity $ Right false)
      test "parses repeated value" $ do
        equal (runExceptT (runValidation ((_."X") <$> prs) ["Y", "Y", "X"])) (Identity $ Right true)
        equal (runExceptT (runValidation ((_."Y") <$> prs) ["Y", "Y", "X"])) (Identity $ Right true)
        equal (runExceptT (runValidation ((_."Z") <$> prs) ["Y", "Y", "X"])) (Identity $ Right false)
