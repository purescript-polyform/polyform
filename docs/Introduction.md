# purescript-polyform

The main goal of this library is to build composable and extensible HTML form representation which handles validation. Current library design is not particularly tight to HTML (as it doesn't provide any render etc.) but it is used and battle tested in this context.

## Objectives

  * Be completely render backend agnostic (form can be rendered as HTML but also as for example... HTTP query).

  * Allow validation to be abstracted over data source, so we can reuse it on the backend or frontend or during typed input validation.

  * Validation should return well typed result value __and form__ value or form filled with errors, so you can always render it.

  * Provide only minimal representations of form fields, which are relevant to validation.


## It's completely OK to hate this desing

This library entangles validation process with form build up which can be unacceptable to you. Additionally I've found it easier to model validation of fields in a different way (`Either`) than forms (something similar to `V` from `purscript-validation`) so you will find two `Validation` types here which can be confusing...

For sure there are even more reasons not to like this code so just feel free to hate this design. But if you have some constructive suggestions how I can improve current situation please open an issue.

