# purescript-polyform

Main objective of this library is to build composable and extensible HTML form representation which handles validation. Current main validation design is not particularly tight to HTML (as it doesn't provide any render etc.) but it is used and battle tested in this context.

## Goals

  * Be completely render backend agnostic (form can be rendered as HTML but also as for example... HTTP query).

  * Validation should abstract over data source, so we can reuse it on the backend or frontend or during typed input validation.

  * Validation should return well typed result value __and form__ value or form filled with errors, so you can always render it.

  * Provide only minimal representation of form fields, which is relevant to validation process and allow users to extend it easily so they can create own fields or extends existing one with additional attributes etc.

  * Provide complete form renderers (for example Bootstrap in Smolder etc.) as external libs.


## Architecture

  The main type of this library is `Validation` which is a function from input value/query to final result and a monoidal "form" value (or field - depends on the particular context). What is interesting (or abusing ;-)) is that eventual validation error is also represented by the same "form" type. In essence our validation result is:

    data V e a = Invalid e | Valid e a

and validation is just a function with additional `Applicative` context `m`:

    data Validtion m e q a = Valiation (q -> m (V e a))

  We can think of `q` as an input data/query, `m` as a computational context, `e` could be our "form" and `a` is a result type of successful validation.

  Having this structure of validation we can combine (using `Applicative` or `Category` instances) these validation functions to produce larger forms even when some of them represents failure. All combined validation functions operate on the same input data like query string or input record in similar way as `Applicative` instance is implemented for `Function`.

  There is also abstraction layer build upon `purescript-run` which is used to facilitate reusability of complex validation scenarios in different data sources contexts. By interpreting this validations (and by using different context `m`) we can for example use the same validation on the backend of a web application (where we have http query as an input) or on a frontend in Javascript application (where we get some record with unparsed values as an input).

  An extra pieces provided by this library are minimal fields' representations (containing only fields related to validation) with some generic helpers. We are going to extend them in this guide to represent something useful so it should not bother you that they are really tiny ;-)

