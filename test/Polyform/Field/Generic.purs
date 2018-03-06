module Test.Polyform.Field.Generic where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.Tuple.Nested ((/\))
import Polyform.Field.Generic (choiceParser, choices, multiChoiceParser)
import Polyform.Field.Generic.Option (type (:-), Nil, Option, option)
import Polyform.Field.Generic.Option as Option
import Polyform.Validation (V(..), Validation(..), runValidation)
import Test.Unit (test)
import Test.Unit as Test.Unit
import Test.Unit.Assert (equal)
import Type.Prelude (Proxy(Proxy), SProxy(..))

data Opts = X | Y | Z
derive instance genericOpts ∷ Generic Opts _
derive instance eqOpts ∷ Eq Opts
instance showOpts ∷ Show Opts where
  show = genericShow

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
suite = do
  Test.Unit.suite "SingleChoice instance for coproduct type" do
    test "generates all choices" $ do
      let opts = choices (Proxy ∷ Proxy Opts)
      equal (("X" /\ X) : ("Y" /\ Y) : ("Z" /\ Z) : Nil) opts
    let prs = choiceParser (Proxy ∷ Proxy Opts)
    test "parses all constructors" $ do
      equal (runValidation prs "X") (Identity $ Valid "X" X)
      equal (runValidation prs "Y") (Identity $ Valid "Y" Y)
      equal (runValidation prs "Z") (Identity $ Valid "Z" Z)
    test "fails for constructor prefix" $ do
      equal (runValidation prs "X'") (Identity $ Invalid ("X'"))
    test "fails for completely wrong value" $ do
      equal (runValidation prs "Unkown") (Identity $ Invalid ("Unkown"))
  Test.Unit.suite "MultiChoice instance" $ do
    Test.Unit.suite "for coproduct type" $ do
      let
        prs = _.product <$> multiChoiceParser (Proxy ∷ Proxy Opts)
      test "parses single value" $ do
        equal (runValidation ((_."X") <$> prs) ["X"]) (Identity $ Valid [] true)
        equal (runValidation ((_."Y") <$> prs) ["X"]) (Identity $ Valid [] false)
        equal (runValidation ((_."Z") <$> prs) ["X"]) (Identity $ Valid [] false)
      test "parses multiple values" $ do
        equal ((runValidation ((_."X") <$> prs) ["X", "Z"])) (Identity $ Valid [] true)
        equal ((runValidation ((_."Y") <$> prs) ["X", "Z"])) (Identity $ Valid [] false)
        equal ((runValidation ((_."Z") <$> prs) ["X", "Z"])) (Identity $ Valid [] true)
      test "parses no values" $ do
        equal ((runValidation ((_."X") <$> prs) [])) (Identity $ Valid [] false)
        equal ((runValidation ((_."Y") <$> prs) [])) (Identity $ Valid [] false)
        equal ((runValidation ((_."Z") <$> prs) [])) (Identity $ Valid [] false)
      test "parses repeated value" $ do
        equal ((runValidation ((_."X") <$> prs) ["Y", "Y", "X"])) (Identity $ Valid [] true)
        equal ((runValidation ((_."Y") <$> prs) ["Y", "Y", "X"])) (Identity $ Valid [] true)
        equal ((runValidation ((_."Z") <$> prs) ["Y", "Y", "X"])) (Identity $ Valid [] false)
      let
        check =
          (_.checkChoice <$> multiChoiceParser (Proxy ∷ Proxy Opts)) ∷ Validation Identity (Array String) (Array String) (Opts → Boolean)
      test "checks values" $ do
        equal (map (_ $ X) <$> runValidation check ["X", "Z"]) (Identity $ Valid [] true)
        equal (map (_ $ Z) <$> runValidation check ["X", "Z"]) (Identity $ Valid [] true)
        equal (map (_ $ Y) <$> runValidation check ["X", "Z"]) (Identity $ Valid [] false)
    Test.Unit.suite "for symbols list" $ do
      let prs = _.product <$> Option.multiChoiceParser (Proxy ∷ Proxy ("X" :- "Y" :- "Z" :- Nil))
      test "parses single value" $ do
        equal ((runValidation ((_."X") <$> prs) ["X"])) (Identity $ Valid [] true)
        equal ((runValidation ((_."Y") <$> prs) ["X"])) (Identity $ Valid [] false)
        equal ((runValidation ((_."Z") <$> prs) ["X"])) (Identity $ Valid [] false)
      test "parses multiple values" $ do
        equal ((runValidation ((_."X") <$> prs) ["X", "Z"])) (Identity $ Valid [] true)
        equal ((runValidation ((_."Y") <$> prs) ["X", "Z"])) (Identity $ Valid [] false)
        equal ((runValidation ((_."Z") <$> prs) ["X", "Z"])) (Identity $ Valid [] true)
      test "parses no values" $ do
        equal ((runValidation ((_."X") <$> prs) [])) (Identity $ Valid [] false)
        equal ((runValidation ((_."Y") <$> prs) [])) (Identity $ Valid [] false)
        equal ((runValidation ((_."Z") <$> prs) [])) (Identity $ Valid [] false)
      test "parses repeated value" $ do
        equal ((runValidation ((_."X") <$> prs) ["Y", "Y", "X"])) (Identity $ Valid [] true)
        equal ((runValidation ((_."Y") <$> prs) ["Y", "Y", "X"])) (Identity $ Valid [] true)
        equal ((runValidation ((_."Z") <$> prs) ["Y", "Y", "X"])) (Identity $ Valid [] false)
      let
        check = _.checkChoice <$> Option.multiChoiceParser (Proxy ∷ Proxy ("X" :- "Y" :- "Z" :- Nil))
      test "checks values" $ do
        equal (map (_ $ (option (SProxy ∷ SProxy "X"))) <$> runValidation check ["X", "Z"]) (Identity $ Valid [] true)
        equal (map (_ $ (option (SProxy ∷ SProxy "Z"))) <$> runValidation check ["X", "Z"]) (Identity $ Valid [] true)
        equal (map (_ $ (option (SProxy ∷ SProxy "Y"))) <$> runValidation check ["X", "Z"]) (Identity $ Valid [] false)
