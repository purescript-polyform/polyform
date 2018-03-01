module Test.Polyform.Input.Http where

import Prelude

import Control.Monad.Aff.Console (log)
import Control.Monad.Except (runExceptT, throwError)
import Data.Array (all, elem)
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
import Polyform.Input.Interpret.Http (StringErr)
import Polyform.Form.Component (liftV, liftMV, runValidation)
import Polyform.Input.Interpret (stringForm)
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

-- | We are going to build sign up form and later a login form
-- | In both cases we need need two types of fields.
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
passwordForm update =
  let
    passwordField' = update passwordField
  in
    Http.fromField
      (fieldForm PasswordInput)
      passwordField'
      (Http.textInputValidation passwordField')
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
signupForm
  = ({email: _, password1: _, password2: _}
  <$> emailForm
  <*> (passwordForm (_{ name = "password1" }))
  <*> (passwordForm (_{ name = "password2" })))
  >>> (liftMV checkEmail *> liftV checkPasswords)
 where
  checkPasswords r@{email, password1, password2 } =
    if password1 == password2
      then pure email
      else Invalid (errorForm "Passwords don't match")

  checkEmail ∷ ∀ m. Monad m ⇒ _ → m _
  checkEmail r@{ email } = do
    -- | Do some effectful stuff like ajax, db query etc.
    pure $ if email `elem` ["user1@example.com", "user2@example.com"]
      then
        Invalid (errorForm "Email already in use")
      else
        pure email

-- | ... and here is another form component.
loginForm =
  ({email: _, password: _} <$> emailForm <*> (passwordForm id)) >>> liftV authenticate
 where
  -- | This db lookup can also be performed in monadic context
  -- | the only difference would be `liftMV` usage.
  db = fromFoldable [Tuple "user1@example.com" "pass", Tuple "user2@example.com" "pass"]
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
  Test.Unit.suite "validation" do
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
        let query = fromFoldable [Tuple "email" [Just "user1@example.com"], Tuple "password" [Just "pass"]]
        v ← runValidation loginForm query
        assert "login successful" (isValid v)
      test "fails otherwise" $ do
        let query = fromFoldable [Tuple "email" [Just "user1@example.com"], Tuple "password" [Just "wrong"]]
        v ← runValidation loginForm query
        assertFalse "login fails" (isValid v)

      test "fails with all form errors aggregated" $ do
        let
          query = fromFoldable
            [ Tuple "email" [Just "user1@example.com"]
            , Tuple "password1" [Just "a"]
            , Tuple "password2" [Just "b"]
            ]
        v ← runValidation signupForm query
        case v of
          Valid _ _ → failure "login successful"
          Invalid (Tuple errors fields) →
            equal errors ["Email already in use", "Passwords don't match"]
        assertFalse "login successful" (isValid v)
