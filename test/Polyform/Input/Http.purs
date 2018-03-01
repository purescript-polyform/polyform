module Test.Polyform.Input.Http where

import Prelude

import Control.Monad.Except (runExceptT, throwError)
import Data.Array (all)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Record (set)
import Data.StrMap (fromFoldable, lookup)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (class VariantEqs, Variant, inj)
import Data.Variant.Internal (class VariantTags)
import Polyform.Field.Generic (choiceParser, choices, multiChoiceParser)
import Polyform.Field.Generic.Option (type (:-), Nil)
import Polyform.Field.Generic.Option as Option
import Polyform.Field.Html5 (textInputValidation)
import Polyform.Field.Html5 as Html5
import Polyform.Field.Validation.Interpret.Http (StringErr)
import Polyform.Form.Component (liftV, runValidation)
import Polyform.Form.Component.Interpret (stringForm)
import Polyform.Form.Validation (V(..), isValid)
import Polyform.Input.Http as Http
import Test.Unit (failure, test)
import Test.Unit as Test.Unit
import Test.Unit.Assert (assert, assertFalse, equal)
import Type.Prelude (Proxy(Proxy), SProxy(..), reflectSymbol)
import Type.Row (class RowToList)

-- | Polyform provides attributes only related to validation
-- | so we are going to extend them.
type ExtraAttrs =
  ( label ∷ String
  , classes ∷ Array String
  , helpText ∷ Maybe String
  )

-- | We are going to build only login form, so we need two types of fields.
-- |
-- | We want also to restrict ourselfs to server side validation
-- | so possible basic field validation errors are limited to this context
-- | by using `Polyform.Field.Validation.Interpret.Http.StringErr`.
-- |
-- | Later we will reuse this form on the frontend and then we
-- | make errors polymorphic and context dependent.
data Field
  = EmailInput (Http.EmailInput ExtraAttrs ())
  | PasswordInput (Http.PasswordInput ExtraAttrs ())


-- | It is time to define our form type and we have chosen... just a `Tuple` :-)
-- | We are going to use `Array` of `Strings` for form level error representation.
-- | `Tuple` provides `Monoid` instance which will append our `Arrays` and it
-- | seems sufficient for our simple example.
type Form = Tuple (Array String) (Array Field)

errorForm err =
  Tuple [err] []

fieldForm constructor record =
  Tuple [] [constructor record]

passwordField =
  { classes: []
  , helpText: Nothing
  , label: "Password"
  , maxlength: Nothing
  , minlength: Just 8
  , name: "password"
  , value: Right ""
  }

passwordForm =
  Http.fromField
    (fieldForm PasswordInput)
    passwordField
    (Http.textInputValidation passwordField)

emailField =
  { classes: []
  , helpText: Nothing
  , label: "Email"
  , maxlength: Nothing
  , minlength: Nothing
  , name: "email"
  , value: Right ""
  }

emailForm =
  Http.fromField
    (fieldForm EmailInput)
    emailField
    (Http.textInputValidation emailField)

-- | This db lookup can also be performed in monadic context
-- | the only difference would be `liftMV` usage.
loginForm =
  ({email: _, password: _} <$> emailForm <*> passwordForm) >>> liftV \{ email, password } →
    if email `lookup` db == Just password
      then
        pure email
      else
        Invalid $ errorForm ("Invalid credentials")
  where
    db = fromFoldable [Tuple "user@example.com" "pass", Tuple "user2@example.com" "pass"]


value ∷ Field → _
value (EmailInput r) = r.value
value (PasswordInput r) = r.value

suite = do
  Test.Unit.suite "Validation" do
    test "of required fields" $ do
      v ← runValidation loginForm mempty
      assertFalse "fails on empty query" (isValid v)
      case v of
        Valid _ _ → failure "should fail on empty input"
        Invalid (Tuple err fields) → do
          assert
            "should fail with \"required\""
            (all (_ == Left (inj (SProxy ∷ SProxy "required") unit)) <<< map value $ fields)
