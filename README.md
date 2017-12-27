# purescript-polyform

Pre α-stage.

## Goals

An attempt to build simple, composable (and opinionated of course) html form validation/representation API...

  * Form should be easly renderable, so you can write really generic renderer and pass it form value (validated or not) and get desired html

  * Validation should return well typed result value __and form__ value or form filled with errors, so you can always generate html for it

  * Provide only minimal representation of html form fields, which is relevant to validation process and allow users to extend it easily so they can create own fields or extends existing one with additional attributes etc.


Caution!

My representation entangles form buildup and validation closely. This has a lot of benfits in terms of minimizing redundancy but I understand that this can be unacceptable for you...


## Validation

This lib extends applicative validation idea a bit further (I mean `purescript-validation`). The idea is to build up a form and validation result during the whole validation process - so form is a result of failed validation but also successful validation steps. So either `Invalid` or `Valid` variant carries `Form` value `e`:

  ```purescript
    data V e a = Invalid e | Valid e a
  ```

This representation allows us to get a form from every validation.


## Fields

Currently you can find this html field representation here:

  ```purescript
  type Input e a attrs =
    { name ∷ String
    , value ∷ Either e a
    | attrs
    }

  type ChoiceField e opt attrs =
    { name ∷ String
    , options ∷ List (Tuple String opt)
    , value ∷ Either e opt
    | attrs
    }

  type MultiChoiceField e opt attrs =
    { name ∷ String
    , choices ∷ List (Tuple String opt)
    , value ∷ Either e (opt → Boolean)
    | attrs
    }
  ```

For `ChoiceField` and `MultiChoiceField` there is "generic" implementation provided which allows you to define these type based on simple sum type or "lists of symbols"...
