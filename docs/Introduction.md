# purescript-polyform

The main goal of this library is to build composable and extensible HTML form representation which handles validation. Current library design is not particularly tight to HTML (as it doesn't provide any render etc.) but it is used and battle tested in this context.

## Objectives

  * Be completely render backend agnostic (form can be rendered as HTML but also as for example... HTTP query).

  * Validation should abstract over data source, so we can reuse it on the backend or frontend or during typed input validation.

  * Validation should return well typed result value __and form__ value or form filled with errors, so you can always render it.

  * Provide only minimal representation of form fields, which is relevant to validation process and allow users to extend it easily so they can create own fields or extends existing one with additional attributes etc.


## Architecture

  It seems that the main type of this library is `Polyform.Form.Validation` which is a function from input value/query to final result and a monoidal "form" value (or field - depends on the particular context). What is important is that eventual validation error are also represented by the same "form" type. In essence our validation result is:

    data V e a = Invalid e | Valid e a

and validation is just a function with additional `Applicative` context `m`:

    data Validtion m e q a = Valiation (q -> m (V e a))

  We can think of `q` as an input data/query, `m` as a computational context, `e` could be our "form" and `a` is a result type of successful validation.

  Having this structure of validation we can combine (using `Applicative` or `Category` instances) multiple validation functions to produce larger and larger forms even when some of these functions fail. All combined validation functions operate on the same input data in similar way as `Applicative` instance is implemented for `Function` type.

  There is also abstraction layer build upon `purescript-run` which is used to facilitate reusability of complex validation scenarios in different data sources contexts. By interpreting this validations (and by using different context `m`) we can for example use the same validation on the backend of a web application (where we have HTTP query as an input) or on a frontend in a Javascript application (where we probably have some record with unparsed values as an input).

  There are also extra pieces provided like minimal fields' representations (containing only validation related content) with some generic helpers. We are going to extend them in this guide to represent something useful so it should not bother you that they are really tiny ;-)


## It's completely OK to hate this desing

This library entangles validation process with form build up which can be unacceptable to you. Additionally I've found it easier to model validation of fields in a different way (just a ffunction with `Either` as a result) than forms (which I've described above) so you will find two `Validation` types here which can be confusing and disgusting for some people...

For sure there are even more reasons not to like this code so just feel free to hate this design and don't bother to use it. But if you have some constructive suggestions how I can improve current situation please open an issue.

