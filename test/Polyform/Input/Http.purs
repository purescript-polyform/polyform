module Test.Polyform.Input.Http where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array (all, catMaybes, elem)
import Data.Either (Either(..))
import Data.Foreign (F)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Record (set)
import Data.StrMap (StrMap, fromFoldable, lookup)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Data.Variant (class VariantEqs, Variant, inj)
import Data.Variant.Internal (class VariantTags)
import Debug.Trace (traceAnyA)
import Network.HTTP.Affjax (AJAX, Affjax, AffjaxResponse, affjax, defaultRequest)
import Polyform.Field.Generic (choiceParser, choices, multiChoiceParser)
import Polyform.Field.Generic.Option (type (:-), Nil)
import Polyform.Field.Generic.Option as Option
import Polyform.Field.Html5 (textInputValidation)
import Polyform.Field.Html5 as Html5
import Polyform.Form.Component (Component(..), hoistFn, hoistFnMV, hoistFnV, runValidation)
import Polyform.Input.Http as Http
import Polyform.Input.Interpret (stringForm)
import Polyform.Validation (V(..), Validation(..), isValid)
import Run (FProxy(..))
import Test.Unit (failure, test)
import Test.Unit as Test.Unit
import Test.Unit.Assert (assert, assertFalse, equal)
import Text.Smolder.HTML.Attributes (maxlength)
import Type.Prelude (Proxy(Proxy), SProxy(..), reflectSymbol)
import Type.Row (class RowToList)
import Unsafe.Coerce (unsafeCoerce)

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
  -- | CheckedEmailInput CheckedEmailInput
derive instance eqField ∷ Eq Field


-- | It is time to define our form type and we have chosen... just a `Tuple` :-)
-- | We are going to use `Array` of `Strings` for form level error representation.
-- | `Tuple` provides `Monoid` instance which will append our `Arrays` and it
-- | seems sufficient for our simple example.
type Form = Tuple (Array String) (Array Field)

-- | Thses two are simple helpers which facilitates form usage
errorForm err =
  Tuple [err] []

fieldForm ∷ ∀ a e r. (r → e) → r → Tuple (Array a) (Array e)
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
  , value: Valid [] ""
  }
emailField =
  { classes: []
  , helpText: Nothing
  , label: "Email"
  , maxlength: Nothing
  , minlength: Nothing
  , name: "email"
  , value: Valid [] ""
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

-- -- | Now we are ready to build our form validation component.
-- -- | When using `Applicative`/`Category` instances to validate
-- -- | inputs we are essentially combining our form values using monoidal
-- -- | append. For example the last step provides a way to
-- -- | "inject" form level errors into our structure, but also
-- -- | allows us to change result type:
-- -- | `{ password1, password2, email } → { password, email }`
signupForm
  ∷ ∀ m
  . Monad m
  ⇒ Component m Form Http.Query { password ∷ String, email ∷ String }
signupForm
  = ({email: _, password1: _, password2: _}
  <$> emailForm
  <*> (passwordForm (_{ name = "password1" }))
  <*> (passwordForm (_{ name = "password2" })))
  >>> (hoistFnMV checkEmail *> hoistFnV checkPasswords)
 where
  checkPasswords r@{ email, password1, password2 } =
    if password1 == password2
      then pure { email, password: password1 }
      else Invalid (errorForm "Passwords don't match")

  checkEmail r@{ email } = do
    -- | Do some effectful stuff like ajax, db query etc.
    pure $ if email `elem` ["user1@example.com", "user2@example.com"]
      then
        Invalid (errorForm "Email already in use")
      else
        pure r

-- | ... and here is another form component.
loginForm =
  ({email: _, password: _} <$> emailForm <*> (passwordForm id)) >>> hoistFnV authenticate
 where
  -- | This db lookup can also be performed in monadic context
  -- | the only difference would be `hoistFnMV` usage.
  db = fromFoldable [Tuple "user1@example.com" "pass", Tuple "user2@example.com" "pass"]
  authenticate { email, password } =
    if email `lookup` db == Just password
      then
        pure email
      else
        Invalid $ errorForm ("Invalid credentials")

-- -- | This is simple scenario which validates email on the form level
-- checkEmail' r@{ email: addr } = do
--   inUse ← email addr
--   pure $ case inUse of
--     Unknown → pure r
--     NotUsed → pure r
--     Used → Invalid (errorForm "Email already in use")
-- 
-- signupForm' ∷ Component EmailInUse Form Http.Query { password ∷ String, email ∷ String }
-- signupForm'
--   = ({email: _, password1: _, password2: _}
--   <$> emailForm
--   <*> (passwordForm (_{ name = "password1" }))
--   <*> (passwordForm (_{ name = "password2" })))
--   >>> (hoistFnMV checkEmail' *> hoistFnV checkPasswords)
--  where
--   checkPasswords r@{email, password1, password2 } =
--     if password1 == password2
--       then pure { password: password1, email }
--       else Invalid (errorForm "Passwords don't match")
-- 
-- validateSignupLocal
--   ∷ forall m
--   . MonadRec m
--   ⇒ Http.Query
--   → m
--     (V Form
--       { password :: String
--       , email :: String
--       })
-- validateSignupLocal query = foldFree interpretLocal $ runValidation signupForm' query
-- 
-- validateSignupRemote
--   ∷ ∀ eff
--   . Http.Query
--   → Aff ( ajax :: AJAX | eff ) (V Form { password :: String , email :: String })
-- validateSignupRemote query = foldFree interpretRemote $ runValidation signupForm' query
-- 
-- 
-- -- | We can use this `Boolean` value
-- -- | when rendering a field to show a throbber etc.
-- type CheckedEmailValue = Tuple Boolean String
-- 
-- -- | We restrict ourselfs to HTTP input here
-- -- | but this error row can be easily parametrized
-- -- | and field could be reused for different types
-- -- | of inputs (this strategy is used for fields
-- -- | defined in `Field.Html5`)
-- type CheckedEmailErr = Variant (Http.StringErr (isUsed ∷ String))
-- 
-- -- | We could use helpers like `Html5.TextInputBase` or functions
-- -- | from `Field.Input`or `Input.Http` here but I want to show
-- -- | that there is no "magic" behind the curtain.
-- -- | Just plain records.
-- -- | Of course we could add additional `attrs` parameter to this record or
-- -- | just add type parameter for this (like in `Field.Html5`).
-- type CheckedEmailInput =
--   { name ∷ String
--   , value ∷ Either CheckedEmailErr CheckedEmailValue
--   }
-- 
-- -- | It is also worth to point out that
-- -- | `value` attribute from our field
-- -- | DOES NOT HAVE TO BE THE SAME as
-- -- | value build by a form containing it.
-- -- | I've just extended helper function and
-- -- | added `Polyform.Component.fromFieldCoerce`
-- -- | to help build form with such a difference
-- -- | on field vs form level.
-- checkedEmailInputValidation
--   ∷ forall err.
--   Validation
--     EmailInUse
--     CheckedEmailErr
--     String
--     (Tuple Boolean String)
-- checkedEmailInputValidation = Field.hoistFnMEither $ \addr → do
--   isUsed ← email addr
--   pure $ case isUsed of
--     Unknown → pure (Tuple false addr)
--     NotUsed → pure (Tuple true addr)
--     -- | Default validators use `Variant` error representation.
--     -- | If we want to easily compose with them
--     -- | we should use it too.
--     Used → Left (inj (SProxy ∷ SProxy "isUsed") addr)
-- 
-- -- | Let's extend our validator so it works on `Http.Value ~ (Array (Maybe String)`.
-- -- | Basically we are preprocessing `Value` into single `String`.
-- checkedEmailInputValidationHttp
--   ∷ ∀ err.
--    Field.Validation
--     EmailInUse
--      -- | All errors which could happend during our validation from `Value`
--      -- | to our final value are here
--      CheckedEmailErr
--      Http.Value
--      (Tuple Boolean String)
-- checkedEmailInputValidationHttp
--   = Field.hoistFn catMaybes
--   >>> Field.required
--   >>> Field.scalar
--   -- | This composition chain stops
--   -- | on the first encountered error.
--   -- | If preceding validations fail
--   -- | we won't run the last one.
--   -- |
--   -- | As a side note we could consider
--   -- | also aggregating scenario
--   -- | like in `Alt` instance for `Except`.
--   -- | Currently there is no `Alt` instance
--   -- | for `Field.Validation` BUT I want to provide
--   -- | at least two instances `AltAcc` (which
--   -- | accumulates errors and would return `Left`
--   -- | when there is at least one error)  and
--   -- | `AltRight` (standard approach - return first `Right`
--   -- | if encountered).
--   >>> checkedEmailInputValidation
-- 
-- -- | Now we are fully armed - we have:
-- -- |   - field representation
-- -- |   - field validation
-- -- |   - form representation
-- checkedEmailForm emailField =
--   Http.fromFieldCoerce
--     coerce
--     (\r → Tuple [] [CheckedEmailInput r])
--     emailField
--     checkedEmailInputValidationHttp
--  where
--   -- | Our final validation result on the form level
--   -- | should be just email without information about
--   -- | validation progress ;-)
--   coerce (Tuple _ v) = v
--   fieldForm constructor record =
--     Tuple [] [constructor record]
-- 
-- data EmailUsage = Used | NotUsed | Unknown
-- 
-- data EmailInUseF a = EmailInUseF String (EmailUsage → a)
-- derive instance functorEmailInUseF ∷ Functor EmailInUseF
-- 
-- type EmailInUse = Free EmailInUseF
-- 
-- email addr = liftF (EmailInUseF addr id)
-- 
-- interpretLocal (EmailInUseF email k) =
--   pure $ k Unknown
-- interpretRemote (EmailInUseF email k) = do
--   -- | let pretend that we are using email and get some response from API
--   (r ∷ AffjaxResponse String) ← affjax $ defaultRequest { url = "http://google.com", method = Left GET }
--   -- | Process response and inform about results ;-)
--   pure $ k Used
-- 
-- signupForm'' ∷ Component EmailInUse Form Http.Query { password ∷ String, email ∷ String }
-- signupForm''
--   = ({email: _, password1: _, password2: _}
--   <$> checkedEmailForm { name: "email", value: Right (Tuple false "") }
--   <*> (passwordForm (_{ name = "password1" }))
--   <*> (passwordForm (_{ name = "password2" })))
--   >>> (hoistFnMV checkEmail' *> hoistFnV checkPasswords)
--  where
--   checkPasswords r@{email, password1, password2 } =
--     if password1 == password2
--       then pure { password: password1, email }
--       else Invalid (errorForm "Passwords don't match")
-- 
-- validateSignupLocal'
--   ∷ forall m
--   . MonadRec m
--   ⇒ Http.Query
--   → m
--     (V Form
--       { password :: String
--       , email :: String
--       })
-- validateSignupLocal' query = foldFree interpretLocal $ runValidation signupForm'' query
-- 
-- validateSignupRemote'
--   ∷ ∀ eff
--   . Http.Query
--   → Aff ( ajax :: AJAX | eff ) (V Form { password :: String , email :: String })
-- validateSignupRemote' query = foldFree interpretRemote $ runValidation signupForm'' query
-- 
-- 
value ∷ Field → _
value (EmailInput r) = r.value
value (PasswordInput r) = r.value
-- value (CheckedEmailInput r) = (unsafeCoerce r.value) >>= (snd >>> pure)

suite = do
  Test.Unit.suite "validation" $ do
    Test.Unit.suite "of required fields" $ do
      test "fails on empty query" do
      v ← runValidation loginForm mempty
      case v of
        Valid _ _ → failure "valid!"
        Invalid (Tuple err fields) → do
          assert
            "with type \"required\""
            (all (_ == Invalid [(inj (SProxy ∷ SProxy "required") unit)]) <<< map value $ fields)

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
          Valid _ _ → failure "signup successful"
          Invalid (Tuple errors fields) →
            equal errors ["Email already in use", "Passwords don't match"]
        assertFalse "login successful" (isValid v)

--       test "coerces value when required" $ do
--         let
--           query = fromFoldable
--             [ Tuple "email" [Just "user1@example.com"]
--             , Tuple "password1" [Just "a"]
--             , Tuple "password2" [Just "a"]
--             ]
--         v ←  foldFree interpretLocal $ runValidation signupForm'' query
--         case v of
--           Valid (Tuple _ fields) value → do
--             equal value.email "user1@example.com"
--             assert "Field not present" ((CheckedEmailInput { name: "email", value: Right (Tuple false "user1@example.com")}) `elem` fields)
--           Invalid (Tuple errors fields) → do
--             failure "signup failure"
