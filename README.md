# purescript-polyform

An attempt to build easy to use and composable validation toolkit. As a bonus it provides a different codec type formulation than in the codecs lib which is called `Dual`.

Sorry for these sparse docs. I hope to extend them soon. Till then it is just better to take a look at the real world examples (json duals, urlencode validators etc.) in the `lambdaterms/purescript-polyform-validators` repo.

## Core types

### `Validator`

This type provides an `Applicative` instance which allow composition in the `Reader` spirit - all `Validators` "work" on the same input. It is accumulative because it is built on top of the `V` type from `purescript-validation`.
There is also a `Category` instance which allows you to build validation chains combining this applicative steps.

```
newtype Validator m e i o = Validator (Star (Compose m (V e)) i o)
```

### `Exceptor`

This type is nearly `Star (Except e m)` but there is no `Semigroup e` constraint in its `Alt` instance (consistent with `Either`).


```
newtype Exceptor m e i o = Exceptor (Star (ExceptT e m) i o)
```

### `Reporter`

Reporter is built on top the `R` type which seems a bit redundant type (isomorphic to just `Tuple r (Maybe a)`):

```
data R r a = Failure r | Success r a
```

This type takes the idea of accumulating errors from the `V` type (from `purescript-validation`) to the boring limit. We can think of `r` value as not only the error representation but as the overall validation "report". These values are accumulated by all interesting instances (`Functor`, `Applicative`) of the `R` type.

When this type can be useful? When we consider for example HTML form rendering we can find that when form validation fails we want to present not only invalid parts of the form. In such a case we want to rerender the whole form. It is convenient to have already validated values in such a case and be able to provide some info based on this partially correct state.

Of course we want to also use our favorite type `Star`. We wrap it in a `Reporter` newtype and provide some additional instances like `Alt` or `Category`:

```
newtype Reporter m e i o = Reporter (Star (Compose m (R e)) i o)
```

### `Dual`

```purescript
data DualD p i o o' = DualD (p i o') (o → i)

newtype Dual p i o = Dual (DualD p i o o)
```

The above types allow to build bidirectional validation and serialization flows. `Polyform.Dual` type thanks to its "diverging" nature (baked by `DualD`) can be used in an applicative manner. Finally it has to be combined into the finall result - a correct `Dual`.


```purescript
  profile = Dual $
    ( { email1: _, email2: _, age: _}
    <$> _.email1 ~ emailDual
    <*> _.email2 ~ emailDual
    <*> _.age ~ ageDual
    )
```

Composibility of this type - I mean `Applicative`, `Semigroup` or `Alt` instances are based on the assumption that `i` (which we can consider as a tokens stream or serialization result) has defined `Semigroupoid` instance. It can be a bit surprising but it is often the case and not a blocker - even for serialization and parsing a `Json` because `Object` can be combined or build in a monoidal way.
Here we have some examples of `Dual` values definition which work with `Json` taken from [polyform-validators test suite](https://github.com/lambdaterms/purescript-polyform-validators/blob/master/test/Duals/Validators/Json.purs). We are using generic utils provided by the validators lib:


```purescript
import Polyform.Dual.Record (build) as Dual.Record
import Polyform.Dual.Variant (case_)
import Polyform.Duals.Validators.Json (arrayOf, boolean, field, int, json, noArgs, number, object, on, string, sum, unit, (:=))

aRecordDual = Dual.Record.build
  $ (SProxy ∷ SProxy "foo") := int
  <<< (SProxy ∷ SProxy "bar") := string
  <<< (SProxy ∷ SProxy "baz") := number

aVariantDual = case_
  # on (SProxy ∷ SProxy "s") string
  # on (SProxy ∷ SProxy "u") unit
  # on (SProxy ∷ SProxy "i") int

-- In a polymorphic case (where validation monad stays unkown) we have to wrap fields in `indentity` because of the "record impredicativity".
aSumDual = sum
  { "S": identity string
  , "I": identity int
  , "B": identity boolean
  , "N": identity number
  , "E": identity noArgs
  , "U": identity unit
  }
```

## Name

Initialy `polyform` was intended to provide some basic utils for HTTP form validation. It was polyomrphic on the form type hence the name. After the last rewrite this name has probably no sens any more, but as you know - "naming is the..."
