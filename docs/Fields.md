
## Basic fields

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

## Html5

I'm also providing richer set of fields in Html5 module... More docs soon...
