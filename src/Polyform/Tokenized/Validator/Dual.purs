module Polyform.Tokenized.Validator.Dual where

import Prelude

import Data.List (List)
import Data.List (singleton) as List
import Data.Validation.Semigroup (V)
import Polyform.Dual (parser, serializer) as Polyform.Dual
import Polyform.Tokenized.Dual (Dual, DualD, dual, parser, serializer) as Tokenized.Dual
import Polyform.Tokenized.Validator (end, liftUntokenized, run) as Tokenized.Validator
import Polyform.Validator (Validator) as Validator
import Polyform.Validator.Dual (Dual) as Polyform.Validator

type Dual m err = Tokenized.Dual.Dual (Validator.Validator m err) m

type DualD m err = Tokenized.Dual.DualD (Validator.Validator m err) m

liftUntokenized ∷ ∀ err i m o. Semigroup err ⇒ Monad m ⇒ err → Polyform.Validator.Dual m err i o → Dual m err i o
liftUntokenized err d = Tokenized.Dual.dual
  (Tokenized.Validator.liftUntokenized err $ Polyform.Dual.parser d)
  (map List.singleton <$> Polyform.Dual.serializer d)

runValidator ∷ ∀ err i o m. Monad m ⇒ Dual m err i o → (List i → m (V err o))
runValidator = Tokenized.Validator.run <<< Tokenized.Dual.parser

runSerializer ∷ ∀ err i o m. Applicative m ⇒ Dual m err i o → (o → m (List i))
runSerializer = Tokenized.Dual.serializer

end :: forall err i m. Monad m => Monoid err => (i → err) -> Dual m err i Unit
end err = Tokenized.Dual.dual
  (Tokenized.Validator.end err)
  (const $ pure mempty)
