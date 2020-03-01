# purescript-polyform

Validation toolkit plus alternative codec type formulation.

Sorry for this sparse docs. I hope to extend them soon. Till then it is just better to take a look at the real world examples (json duals etc.) in the `lambdaterms/purescript-polyform-validators` repo.

## Core types

### Validator

It provides an `Applicative` instance which allow compose muptiple validators in a similar the `Reader` spirit. There is also `Category` instance which allows you to build validation chains.

```
newtype Validator m e i o = Validator (Star (Compose m (V e)) i o)

```

### `Exceptor`

This type is nearly `Star (Except e m)` but there is no `Semigroup e` constraint in its `Alt` instance (consistent with `Either`).


```
newtype Exceptor m e i o = Exceptor (Star (ExceptT e m) i o)
```

### `Dual`

Additionally it provides basic types for bidirectional validation and serialization through `Polyform.Dual` type.

```purescript
data DualD p i o o' = DualD (p i o') (o → i)

newtype Dual p i o = Dual (DualD p i o o)
```

## Name

 `polyform` initialy was intended to provide some basic mechanism for html form validation so thats the source of the name of this lib.
