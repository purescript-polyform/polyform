module Test.Main where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Validation.Polyform.Http (asOptions', asParser, asRepParser, choicesParser, choicesParser', toOptionValue')
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Output.Fancy (runTest)
import Type.Prelude (Proxy(..))

data Opts = X | Y | Z
derive instance genericOpts ∷ Generic Opts _
derive instance eqOpts ∷ Eq Opts
instance showOpts ∷ Show Opts where
  show = genericShow

infixr 6 type Tuple as ∧

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
main = launchAff $ runTest $ do
  suite "AsOptions" do
    test "all options are generated" $ do
      let opts = asOptions' (Proxy ∷ Proxy Opts)
      equal (("X" /\ X) : ("Y" /\ Y) : ("Z" /\ Z) : Nil) opts
    let prs = asParser (Proxy ∷ Proxy Opts)
    test "parser parses constructors" $ do
      equal (runExceptT (unwrap prs "X")) (Identity $ Right X)
      equal (runExceptT (unwrap prs "Y")) (Identity $ Right Y)
      equal (runExceptT (unwrap prs "Z")) (Identity $ Right Z)
    test "parser fails for constructor prefix" $ do
      equal (runExceptT (unwrap prs "X'")) (Identity $ Left "error")
    test "parser fails for wrong value" $ do
      equal (runExceptT (unwrap prs "Unkwown")) (Identity $ Left "error")
  suite "Choices type class" $ do
    let prs = choicesParser' (Proxy ∷ Proxy Opts)
    test "parses single value" $ do
      equal (runExceptT (unwrap ((_."X") <$> prs) ["X"])) (Identity $ Right true)
      equal (runExceptT (unwrap ((_."Y") <$> prs) ["X"])) (Identity $ Right false)
      equal (runExceptT (unwrap ((_."Z") <$> prs) ["X"])) (Identity $ Right false)
    test "parses multiple values" $ do
      equal (runExceptT (unwrap ((_."X") <$> prs) ["X", "Z"])) (Identity $ Right true)
      equal (runExceptT (unwrap ((_."Y") <$> prs) ["X", "Z"])) (Identity $ Right false)
      equal (runExceptT (unwrap ((_."Z") <$> prs) ["X", "Z"])) (Identity $ Right true)
    test "parses no values" $ do
      equal (runExceptT (unwrap ((_."X") <$> prs) [])) (Identity $ Right false)
      equal (runExceptT (unwrap ((_."Y") <$> prs) [])) (Identity $ Right false)
      equal (runExceptT (unwrap ((_."Z") <$> prs) [])) (Identity $ Right false)
    test "parses repeated value" $ do
      equal (runExceptT (unwrap ((_."X") <$> prs) ["Y", "Y", "X"])) (Identity $ Right true)
      equal (runExceptT (unwrap ((_."Y") <$> prs) ["Y", "Y", "X"])) (Identity $ Right true)
      equal (runExceptT (unwrap ((_."Z") <$> prs) ["Y", "Y", "X"])) (Identity $ Right false)


