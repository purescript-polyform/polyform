# purescript-polyform

An attempt to build simple, composable html form validation API... 

Pre α-stage.

## Goals

  * Form should be easly renderable, so you can write really generic renderer and pass it form value (validated or not) and get desired html

  * Validation should return well typed result value __and form__ value or form filled with errors, so you can always generate html for it

  * Provide only minimal representation of html form fields, which is relevant to validation process and allow users to extend it easily so they can create own fields or extends existing one with additional attributes etc.


## Current status

This lib extends applicative validation idea a bit further (I mean `purescript-validation`). The idea is to build up a form and validation result during the whole validation process - so form is a result of failed validation but also successful validation steps. So either `Invalid` or `Valid` variant carries `Form` value `e`:

  ```purescript
    data V e a = Invalid e | Valid e a
  ```

Current (used as proof of concept) form type is just product of (form) errors and fields (fields carry their own validation error):


  ```purescript
    data Form = Form (Array String) (Array Field)
  ```

Form error representation will change soon probably to just `StrMap`...

During validation process forms are combined by `Applicative` instance and by `Semigroup` instance, so we are able to build more complex validation scenarios and compose forms easily.
After validation we are going to get form value and in case of validation success also a final value:

  ```purescript
    newtype Profile = Profile
      { nickname ∷ String
      , bio ∷ String
      , age ∷ Int
      , password ∷ String
      }
    derive instance genericProfile ∷ Generic Profile _
    instance showProfile ∷ Show Profile where
      show = genericShow

    profile :: forall m.  Monad m => Validation m Form Query String
    passwordV =
      (Tuple <$> password "password1" "Password" <*> password "password2" "Password (repeat)")
      >>> check "passwords" (pure "Password do not match") (\p → pure $ fst p == snd p)
      >>> pureV (\p → fst p)

    profile :: forall m.  Monad m => Validation m Form Query Profile
    profile =
      Profile <$>
        ({nickname: _, bio: _, age: _, password: _}
          <$> input "nickname" "Nickname"
          <*> input "bio" "Bio"
          <*> number "age" "Age"
          <*> passwordV)
   ```

   ``` purescript
    validateAndPrint passwordV (fromFoldable [Tuple "password1" [Just "admin"]])

    (Form [] [(Input { label: "Password", name: "password1", value: (Val (Just "admin")) }),(Input { label: "Password (repeat)", name: "password2", value: (Err "Appropriate error message..." "") })])

    validateAndPrint passwordV (fromFoldable [Tuple "password1" [Just "admin"], Tuple "password2" [Just "pass"]])

    (Form ["Password do not match"] [(Input { label: "Password", name: "password1", value: (Val (Just "admin")) }),(Input { label: "Password (repeat)", name: "password2", value: (Val (Just "pass")) })])

    validateAndPrint passwordV (fromFoldable [Tuple "password1" [Just "secret"], Tuple "password2" [Just "secret"]])

    (Form [] [(Input { label: "Password", name: "password1", value: (Val (Just "secret")) }),(Input { label: "Password (repeat)", name: "password2", value: (Val (Just "secret")) })])


    let
      onlyNickname =
        (fromFoldable
          [Tuple "nickname" [Just "nick"]])
    validateAndPrint profile onlyNickname

    let
      nicknameAndPassword =
        (fromFoldable
          [ Tuple "nickname" [Just "nick"]
          , Tuple "password1" [Just "new"]
          , Tuple "password2" [Just "new"]])

    validateAndPrint profile nicknameAndPassword

    let
      nicknameAndPasswordMismatch =
        (fromFoldable
          [ Tuple "nickname" [Just "nick"]
          , Tuple "password1" [Just "wrong"]
          , Tuple "password2" [Just "new"]])

    validateAndPrint profile nicknameAndPasswordMismatch

    let
      fullProfile =
        (fromFoldable
          [ Tuple "nickname" [Just "nick"]
          , Tuple "bio" [Just "bio"]
          , Tuple "age" [Just "666"]
          , Tuple "password1" [Just "new"]
          , Tuple "password2" [Just "new"]])

    validateAndPrint profile fullProfile

    (Form [] [(Password { label: "Password", name: "password1", value: (Val (Just "admin")) }),(Password { label: "Password (repeat)", name: "password2", value: (Err "Appropriate error message..." "") })])


    (Form ["Password do not match"] [(Password { label: "Password", name: "password1", value: (Val (Just "admin")) }),(Password { label: "Password (repeat)", name: "password2", value: (Val (Just "pass")) })])


    "secret"
    (Form [] [(Password { label: "Password", name: "password1", value: (Val (Just "secret")) }),(Password { label: "Password (repeat)", name: "password2", value: (Val (Just "secret")) })])


    (Form [] [(Input { label: "Nickname", name: "nickname", value: (Val (Just "nick")) }),(Input { label: "Bio", name: "bio", value: (Err "Appropriate error message..." "") }),(Number { label: "Age", name: "age", value: (Err "Appropriate error message..." "") }),(Password { label: "Password", name: "password1", value: (Err "Appropriate error message..." "") }),(Password { label: "Password (repeat)", name: "password2", value: (Err "Appropriate error message..." "") })])


    (Form [] [(Input { label: "Nickname", name: "nickname", value: (Val (Just "nick")) }),(Input { label: "Bio", name: "bio", value: (Err "Appropriate error message..." "") }),(Number { label: "Age", name: "age", value: (Err "Appropriate error message..." "") }),(Password { label: "Password", name: "password1", value: (Val (Just "new")) }),(Password { label: "Password (repeat)", name: "password2", value: (Val (Just "new")) })])


    (Form ["Password do not match"] [(Input { label: "Nickname", name: "nickname", value: (Val (Just "nick")) }),(Input { label: "Bio", name: "bio", value: (Err "Appropriate error message..." "") }),(Number { label: "Age", name: "age", value: (Err "Appropriate error message..." "") }),(Password { label: "Password", name: "password1", value: (Val (Just "wrong")) }),(Password { label: "Password (repeat)", name: "password2", value: (Val (Just "new")) })])


    (Profile { age: 666, bio: "bio", nickname: "nick", password: "new" })
    (Form [] [(Input { label: "Nickname", name: "nickname", value: (Val (Just "nick")) }),(Input { label: "Bio", name: "bio", value: (Val (Just "bio")) }),(Number { label: "Age", name: "age", value: (Val (Just 666)) }),(Password { label: "Password", name: "password1", value: (Val (Just "new")) }),(Password { label: "Password (repeat)", name: "password2", value: (Val (Just "new")) })])
    ```
