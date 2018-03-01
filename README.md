# purescript-polyform

An attempt to build simple, composable form validation toolkit.

## Objectives

  * Be completely render backend agnostic (form can be rendered as HTML but also as for example... HTTP query).

  * Allow validation to be abstracted over data source, so we can reuse it on the backend or frontend or during typed input validation.

  * But also provide modules dedicated for handling HTTP input and untyped record input.

  * Validation should produce well typed result value __and form__ structure or form filled with errors, so you can always render it.

  * Exact structure of form and fields are up to the user. The only constraint is that form should be a `Monoid`.

  * Provide only minimal representation of form fields with attributes which are relevant to validation.

## Validation overview

### Form Validation

  In `Polyform.Form.Validation` you can find `Validation` type which is a function which in case of success produces a result and a monoidal "form" value. What is really important in case of validation error it also produces "form" value as a failure representation (this allows us to for example always render a form). In essence our validation result is this type:

  ```purescript
    data V e a = Invalid e | Valid e a
  ```

and validation is just a function with additional `Applicative` context `m`:

  ```purescript
    data Validtion m e q a = Valiation (q -> m (V e a))
  ```
  We can think of `q` as an input data/query, `m` as a computational context, `e` could be our "form" and `a` is a result type of successful validation.

  Having this structure of validation we can combine (using `Applicative` or `Category` instances) multiple validation functions to produce larger and larger forms even when some of these functions fail. All combined validation functions operate on the same input data in similar way as `Applicative` instance is implemented for `Function` type.

### Field Validation

  Field validation is build upon `Either` so it short circuit on the first error. It is defined using `Star`, `ExceptT` but it is just a function from input into `Either e a` in monadic context:

  ```purescript
    newtype Validation m e a b = Validation (Star (ExceptT e m) a b)
  ```

## Usage

This scenario is just starting point for full length guide which is under developement.

We are going to build login form which will be tested against HTTP input (full code is in `test/Polyform/Input/Http.purs`). We will use helpers from `Polyform.Input.Http` to simplify our work.

  ```purescript
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

    -- | It is time to define our form type. Let's represent it as a... `Tuple` :-)
    -- | We are going to use `Array` of `Strings` for form level error representation.
    -- | `Tuple` provides `Monoid` instance which appends our `Arrays` and it
    -- | seems sufficient for our simple example.
    type Form = Tuple (Array String) (Array Field)

    -- | Thses two are simple helpers which facilitates form usage
    errorForm err =
      Tuple [err] []

    fieldForm constructor record =
      Tuple [] [constructor record]

    -- | By using `fromField` helper (from `Polyform.Input.Http`)
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
    -- | our forms we are essentially joining our form values using monoidal
    -- | append. For example the last `authenticate` step
    -- | "injects" form level errors into our structure.
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


    -- | We can now use our form to proceed with validation
    main = do
      let query = fromFoldable [Tuple "email" [Just "user@example.com"], Tuple "password" [Just "pass"]]
      v ← runValidation loginForm query
      case v of
        Valid form email → do
          traceAnyA form
        Invalid form → do
          traceAnyA form
  ```

We are getting this debug representation on the console for correct data:


  ``` purescript
    Tuple {
      value0: [],
      value1:
       [ EmailInput {
           value0:
            { classes: [],
              helpText: Nothing {},
              label: 'Email',
              maxlength: Nothing {},
              minlength: Nothing {},
              name: 'email',
              value: Right { value0: 'user@example.com' } } },
         PasswordInput {
           value0:
            { classes: [],
              helpText: Nothing {},
              label: 'Password',
              maxlength: Nothing {},
              minlength: Just { value0: 8 },
              name: 'password',
              value: Right { value0: 'pass' } } } ] }
  ```

and this in case of failure for this data `query = fromFoldable [Tuple "password" []]`:

  ```purescript
      Tuple {
        value0: [],
        value1:
         [ EmailInput {
            ...
                value: Left { value0: { type: 'required', value: {} } } } },
           PasswordInput {
            ...
                value: Left { value0: { type: 'required', value: {} } } } } ] }
  ```

and  for this incorrect credentials:

  ```purescript
  query = fromFoldable [Tuple "email" [Just "user@example.com"], Tuple "password" [Just "wrong"]]
  ```

we are getting this:

    ```purescript
      Tuple {
        value0: [ 'Invalid credentials' ],
        value1:
         [ EmailInput {
             value0:
              ...
                value: Right { value0: 'user@example.com' } } },
           PasswordInput {
             value0:
                ...
                value: Right { value0: 'wrong' } } } ] }
    ```



## API Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-polyform).

## TODO

- Provide multiple `Alt` implementations for `Field.Validation`.

- Add all missing `Html5` fields.

- Test how this design would work with indexed applicative and non monoidal forms.
