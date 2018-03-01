module Test.Polyform.Form.Component where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Record (set)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Polyform.Field (runValidation)
import Polyform.Field.Generic (choiceParser, choices, multiChoiceParser)
import Polyform.Field.Generic.Option (type (:-), Nil)
import Polyform.Field.Generic.Option as Option
import Polyform.Field.Html5 (textInputValidation)
import Polyform.Field.Html5 as Html5
import Polyform.Field.Validation.Interpret.Http (StringErr)
import Polyform.Form.Component.Interpret (stringForm)
import Polyform.Form.Component (liftV)
import Polyform.Form.Validation (V(..))
import Polyform.Http as Http
import Test.Unit (test)
import Test.Unit as Test.Unit
import Test.Unit.Assert (equal)
import Type.Prelude (Proxy(Proxy), SProxy(..), reflectSymbol)

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

formError err =
  Tuple [err] []

fieldForm constructor record =
  Tuple [] [constructor record]

loginForm =
  ({email: _, password: _} <$> emailForm <*> passwordForm) >>> liftV \{ email, password } →
    if email == "user@example.com" && password == "secret"
      then
        pure email
      else
        Invalid $ formError ("Invalid credentials")
 where
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


-- suite = do
--   Test.Unit.suite "SingleChoice instance for coproduct type" do
--     test "generates all choices" $ do
--       let opts = choices (Proxy ∷ Proxy Opts)
--       equal (("X" /\ X) : ("Y" /\ Y) : ("Z" /\ Z) : Nil) opts
