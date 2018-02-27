## Validations strategies

This library provides two types of validation.

### Form Validation

  In `Polyform.Form.Validation` you can find `Validation` type which is a function which produces either a result and a monoidal "form" value in case of success. What is important is that eventual validation error is also represented by the same "form" type. In essence our validation result is:

    data V e a = Invalid e | Valid e a

and validation is just a function with additional `Applicative` context `m`:

    data Validtion m e q a = Valiation (q -> m (V e a))

  We can think of `q` as an input data/query, `m` as a computational context, `e` could be our "form" and `a` is a result type of successful validation.

  Having this structure of validation we can combine (using `Applicative` or `Category` instances) multiple validation functions to produce larger and larger forms even when some of these functions fail. All combined validation functions operate on the same input data in similar way as `Applicative` instance is implemented for `Function` type.

### Field Validation

  Field validation is build upon `Either` so it short circuit on the first error. It is also a function 

  There is also abstraction layer build upon `purescript-run` which is used to facilitate reusability of complex validation scenarios in different data sources contexts. By interpreting this validations (and by using different context `m`) we can for example use the same validation on the backend of a web application (where we have HTTP query as an input) or on a frontend in a Javascript application (where we probably have some record with unparsed values as an input).

  There are also extra pieces provided like minimal fields' representations (containing only validation related content) with some generic helpers. We are going to extend them in this guide to represent something useful so it should not bother you that they are really tiny ;-)


