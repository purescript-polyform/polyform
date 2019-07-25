# purescript-polyform

An attempt to build simple, composable validation toolkit.

## Overview.

### There is no `M****` here!

The whole library is an extension over well known `Applicative` validation strategy which gives us the ability to collect all errors (from a single "step") not only first one like it is in case of monadic approach to validation. `Applicative` also gives us parallelism "for free". It should not be a surprise that half of the library is built on top of the `V` type from `purescript-validation`. Another half is built on top of really similar type `R` (aka `Report`) defined here.

I think that hugely undervalued class in the context of validation is `Category` (and `Semigroupoid`). This library explores extensively types which provide both `Applicative` and  `Category` instances. `Category` in the context of validation can somewhat replace missing `Monad` - we can stack validators and build up some results by combining and validating parital results from previous steps. All validators and parsers types have both of them.

Here we have a simple example of a chain which contains some ready to use validators taken from `purescript-polyform-validators` which demonstrates this idea:

  ```purescript
  import Test.Cookbook.Validators.Affjax (affjax, json, status)
  import Test.Cookbook.Validators.Json (field, int, string)

  -- getUser ∷ Validation Aff (Variant
  getUser
    = affjax "http://api.example.com/user/1"
    >>> status (eq 200)
    >>> json
    >>> { fullName: _, age: _, planet: _ }
      <$> field "full_name" string
      <*> field "age" int
      <*> field "planet" (string >>> hoistFnV \c → c in ["Earth", "Mars", "Venus"])
  ```

You can see that with a little help from `purescript-variant` we are able to construct modular validation solution - we are composing validators for different layers with consistent error handling. In this case we get a single function from request to a final value which processes the whole request stack.
Of course we could introduce multiple shortcuts to this chain (like predefined `affjaxJson`) but this is not the purpose of this example...

Let's go back to the basics and look at the types provided by this library. We will see that we are playing with nothing more here then just functions...



## Basic Types

### Module `Polyform.Validator`

#### `Data.Validation.Semigroup.V`

This type uses `V` from `purescript-validation` so let's start by explaining what special about `V`. `V` is really similar ("isomorphic") to `Either` (to be honest current library implementation just wraps `Either` but we use the old definition here for clarity):

```purescript
data V e a = Invalid e | Valid a
```

The main difference from `Either` is that it doesn't have a `Monad` instance. It doesn't implement it so it can have different `Applicative` instance than `Either` (because `Applicative` has to be "consistent" with `Monad` if it exists).
`Either` stops evaluation of the first error, so for example this:

```purescript
{ fullName: _, age: _ } <$> Left e1 <*> Left e2
```

results in `Left e1`.

If we take `V` into account its `Applicative` instance requires that our `error` type has a `Monoid` instance so it can combine errors during applying a function. So this:

```purescript
{ fullName: _, age: _ } <$> Invalid [e1] <*> Invalid [e2]
```
results in `Invalid [e1, e2]`. Please check ["Purescript by Example"](https://leanpub.com/purescript/read#leanpub-auto-applicative-validation) and [purescript-validation](/purescript/purescript-validation) for more info.

There is no magic here. `Apply` which hold the laws can be simply implemented like this:
```purescript
instance applyV ∷ (Semigroup e) ⇒ Apply (V e) where
  apply (Valid f) (Valid a) = Valid (f a)
  apply (Invalid e1) (Invalid e2) = Invalid (e1 <> e2)
  apply i@(Invalid e1) _ = i
  apply _ i = i
```

#### `Polyform.Validator.Validator`

Now we can discuss main type of this module which is built on top of the above `V` type:

```purescript
newtype Validator m e i o = Validator (i → m (V e o))
```

As you can see `Validator` is a function from input `i` into output `o`. Validation is executed in some monadic context `m` and it can possibly fail and return an error value `e` instead of `o`.


### Module `Polyform.Reporter`



## Cookbook

All examples in this cookbook are somewhat verbose because they are transformed into PureScript modules which we run as a part of the test suite.
On the other hand they are complete and we can be sure that they are up to date.
If you want to generate real modules just run: `npm run test` and check `./test/Cookbook` directory.


* REST API client from scratch - here we are building simple chain of validators so we are able to fetch data from 


* API client + server side data serializer


