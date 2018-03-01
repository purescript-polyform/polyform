module Test.Polyform.Input.Http where

import Prelude

import Control.Monad.Aff.Console (log)
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
import Debug.Trace (traceAnyA)
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

-- | Polyform provides only attributes of fields related to validation
-- | (all fields are records with at least `name`, `value`).
-- | but we can easily extend them. Let's assume that we want these
-- | additional properties:
type ExtraAttrs =
  ( label ∷ String
  , classes ∷ Array String
  , helpText ∷ Maybe String
  )

-- | We are going to build login form, so we need two types of fields.
-- | They can of course be used in different forms too.
data Field
  = EmailInput (Http.EmailInput ExtraAttrs ())
  | PasswordInput (Http.PasswordInput ExtraAttrs ())

-- | It is time to define our form type and we have chosen... just a `Tuple` :-)
-- | We are going to use `Array` of `Strings` for form level error representation.
-- | `Tuple` provides `Monoid` instance which will append our `Arrays` and it
-- | seems sufficient for our simple example.
type Form = Tuple (Array String) (Array Field)

-- | Thses two are simple helpers which facilitates form usage
errorForm err =
  Tuple [err] []

fieldForm constructor record =
  Tuple [] [constructor record]

-- | And now it is time to define our fields defaults
-- | which can be used in forms.
-- | During validation process ONLY `value` FIELD
-- | would be updated and changed according to the
-- | validation result.
-- | It's type for these "text fields" is
-- | (Either (Variant errors) String).
passwordField =
  { classes: []
  , helpText: Nothing
  , label: "Password"
  , maxlength: Nothing
  , minlength: Just 8
  , name: "password"
  , value: Right ""
  }
emailField =
  { classes: []
  , helpText: Nothing
  , label: "Email"
  , maxlength: Nothing
  , minlength: Nothing
  , name: "email"
  , value: Right ""
  }

-- | By using simple helper (in this case from `Polyform.Input.Http`)
-- | we are building single field forms.
-- | This helper takes:
-- |  * "form constructor" which builds form from field record
-- |  * field record
-- |  * field validation
-- | We are using basic text field validation here, but we are going
-- | to extend it later.
passwordForm =
  Http.fromField
    (fieldForm PasswordInput)
    passwordField
    (Http.textInputValidation passwordField)
emailForm =
  Http.fromField
    (fieldForm EmailInput)
    emailField
    (Http.textInputValidation emailField)

-- | Now we are ready to build our form validation component.
-- | When using `Applicative`/`Category` instances to combine/process
-- | we are essentially combining our form values using monoidal
-- | append, so for example the last step provides a way to
-- | "inject" form level errors into our structure.
loginForm =
  ({email: _, password: _} <$> emailForm <*> passwordForm) >>> liftV authenticate
  where
    -- | This db lookup can also be performed in monadic context
    -- | the only difference would be `liftMV` usage.
    db = fromFoldable [Tuple "user@example.com" "pass", Tuple "user2@example.com" "pass"]
    authenticate { email, password } =
      if email `lookup` db == Just password
        then
          pure email
        else
          Invalid $ errorForm ("Invalid credentials")

value ∷ Field → _
value (EmailInput r) = r.value
value (PasswordInput r) = r.value

suite = do
  Test.Unit.suite "Validation" do
    Test.Unit.suite "of required fields" $ do
      test "fails on empty query" $ do
        v ← runValidation loginForm mempty
        case v of
          Valid _ _ → failure "valid!"
          Invalid (Tuple err fields) → do
            assert
              "with type \"required\""
              (all (_ == Left (inj (SProxy ∷ SProxy "required") unit)) <<< map value $ fields)
    Test.Unit.suite "on the form level" $ do
      test "succeeds when form condition is fulfilled" $ do
        let query = fromFoldable [Tuple "email" [Just "user@example.com"], Tuple "password" [Just "pass"]]
        v ← runValidation loginForm query
        assert "login successful" (isValid v)
      test "fails otherwise" $ do
        let query = fromFoldable [Tuple "email" [Just "user@example.com"], Tuple "password" [Just "wrong"]]
        v ← runValidation loginForm query
        assertFalse "login fails" (isValid v)

