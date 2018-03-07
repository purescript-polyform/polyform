# purescript-polyform

An attempt to build simple, composable form validation toolkit.

## Objectives

  * Be completely render backend agnostic (form can be rendered as HTML but also as for example... HTTP query).

  * Allow validation to be abstracted over data source, so we can reuse it on the backend or frontend or during typed input validation.

  * But also provide modules dedicated for handling HTTP input and untyped record input.

  * Validation should produce well typed result value __and form__ structure or form filled with errors, so you can always render it.

  * Exact structure of form and fields are up to the user. The main constraint is that form should be a `Monoid` (it may sound limiting at first but there are also monoids like... `Join (->) a` ;-)).

  * Provide only minimal representation of form fields with attributes which are relevant to validation.

## Validation overview

### Types

  In `Polyform.Validation` you can find `Validation` type - a function which in case of a success produces final result and a monoidal "form" value. In case of a validation error it also produces "form" value as a failure representation (this allows us for example to always render our form). Our validation result has type:

  ```purescript
    data V e a = Invalid e | Valid e a
  ```

and `Validation` is just a function with additional `Monadic` context `m`:

  ```purescript
    data Validation m e q a = Validation (q -> m (V e a))
  ```
  We can think of `q` as an input data/query, `m` as a computational context, `e` could be our "form" and `a` is a result type of successful validation.

  Having this structure of validation we can combine (using `Applicative` or `Alt` or `Category` instances) multiple validation functions to produce larger and larger forms even when some of these functions fail.

  All combined (using `Applicative` or `Alt`) validation functions operate on the same input data in similar way as `Applicative` instance is implemented for `Function` type.

  Values glued together through `Category` composition form a processing chain. Inside this chain you can always append "errors" (or any information) to your "form" value. In this manner we are able to perform additional checking, validation etc.

### Quick example

By using these three instances you can build quite complicated scenarios of form validation. I'm going to provide really brief overview of form construction here.

At first let choose our `Form` representation - `Tuple` provides us `Monoid` instance for free and we can use `Array String` as simple container for form level errors:

```purescript
type Form = Tuple (Array String) (Array Field)
```

In general polyform doesn't really care how you represent the form. It only requires a `Monoid` instance. There are some stubs provided (just records) for `HTML5` fields with related validation functions. Of course you can ignore them and write your own fields and validations if you need different representation.

This library implements also other helpers for other "data sources" like `Polyform.Input.Foreign` or `Polyform.Input.Http`. There is even basic `Polyform.Input.Interpret` module which can help you build really general form and interpret it in different contexts (forexample reuse it on frontend and backend).

As I've said provided fields are really minimal and we are going to use them for simplicity. You can always extend them too if you need:

```purescript
import Polyform.Input.Http as Http

data Field
  -- | We are able to provide here additional fields row and additional errors row
  = EmailInput (Http.EmailInput () ())
  | PasswordInput (Http.PasswordInput () ())

-- | For sure you would like more customization
-- | in this field constructor to reuse this
-- | field in different forms.
-- | But let's keep it simple.
passwordField name =
  { maxlength: Nothing
  , minlength: Just 8
  , name: name
  , value: Valid [] ""
  }
```
We are going to use form construction helper which is a function which glues together validation and default form value and in this case fetches data from HTTP query based on the value from `name` attribute of field record.

``` purescript
buildPasswordForm ∷ String → Validation m Form Query String
buildPasswordForm name =
  Http.fromField
    -- | Here we have to provide single field form "constructor"
    (\r → Tuple [] [PasswordInput r])
    -- | Here goes our field
    (passwordField name)
    -- | And here is our valiation function of this field
    (Http.textInputValidation passwordField')
```

This particular helper produces a validation function from `Polyform.Input.Http.Query` to `String` (which is our `password` value) and accompanying `Form`. In case of validation failure we are going to get just a `Form` with field filled with errors. Representation chosen for this type of fields (from `Field.Html5`) is a list of `Variant` values so you can always extend predefined validations and add your own errors values.

Strategy used by validators in case of plain input fields is simple. They only update `value` attribute from your default record according to the validation result. Of course your etire form also gives you ways to access (through `Functor`, `Applicative`, `Category`) potential results of validation so here we are using `Applicative` to collect values from two fields:

``` purescript
passwordsForm =
  { password1: _, password2: _ }
    <$> (buildPasswordForm "password1")
    <*> (buildPasswordForm "password2")
```

This `passwordForm` value is a validation which produces a record in case of success but also a "sum" of both subforms. We can use another form of composition to process this form


``` purescript
passwordForm = passwordsForm >>> (hoistFnV \{ password1, password2 } →
  if password1 == password2
    -- | Here we are able to use just applicative `pure`
    then Valid mempty password1
    else Invalid (Tuple ["Password doesn't match"] [])
```

`hoistFnV` lifts function `a → V e b` into `Validation m e a b` it is just: ```hoistFnV f = Validation $ f >>> pure ```

So now we have a form which validates if two provided passwords are the same and we are getting single `String` value in case of success.

When we are writing our validation we are allowed to any monad as validation context so for example you can hit db to check if `email` is in use or use `ajax` etc. (or define this as effect using `puerscript-run` and interpret this check in different contexts ;-).

```purescript
emailField =
  { maxlength: Nothing
  , minlength: Nothing
  , name: "email"
  , value: Valid [] ""
  }

emailForm =
  Http.fromField
    (fieldForm EmailInput)
    emailField
    (Http.textInputValidation emailField >>> emailInUse)


-- | Custom field validation function.
-- | F
emailInUse = Validation $ \email → do
  -- | Some monadic action
  inUse ← checkIfEmailUsed email
  pure $ if inUse
    -- | In case of these fields we
    -- | are using list of extensible `Variant`
    -- | to represent errors.
    -- | Don't be affraid of variants their are
    -- | really handy!!!
    else Invalid $ [inj (SProxy :: SProxy "emailInUse") email]
    else Valid [] email
```

Of course we are able to combine these forms and build a final form:

```purescript
signupForm = {email: _, password: _} <$> emailForm <*> passwordForm
```

There are more examples for example in `test/Polyform/Input/Http.purs`. I'm going to provide full guide soon. I promise.

### Parallel execution

There is simple wrapper which allows you to execute validations in "parallel" using your underling monad parallelism - check `Polyform.Validation.Par`. Shortly you can build parrallel execution of validation tree using `<|>` or `<*>` for example like this:

```purescript
sequential $ { email: _, password: _} <$> parallel emailForm <*> parallel passwordForm
```

You have to use `sequential` and `parallel` from `Polyform.Validation.Par` as I'm not able to implement `Parallel` instance for non monad (`Validation` doesn't form a `Monad`).


## API Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-polyform).
