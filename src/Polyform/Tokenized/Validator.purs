module Polyform.Tokenized.Validator where

import Prelude
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V)
import Polyform.Tokenized (Tokenized, liftUntokenized) as Tokenized
import Polyform.Tokenized (unliftUntokenized)
import Polyform.Validator (Validator, liftFnEither, runValidator) as Validator

type Validator m err
  = Tokenized.Tokenized (Validator.Validator m err)

liftUntokenized ∷ ∀ err i o m. Monad m ⇒ Semigroup err ⇒ err → Validator.Validator m err i o → Validator m err i o
liftUntokenized e v = Tokenized.liftUntokenized (v <<< l)
  where
  l =
    Validator.liftFnEither case _ of
      Just i → Right i
      Nothing → Left e

run ∷ ∀ err i m o. Functor m ⇒ Validator m err i o → List i → m (V err o)
run = Validator.runValidator <<< unliftUntokenized

-- | TODO: Do we want to handle end as a part of serializer type
-- | so it is more isomorphic?
end :: forall err i m. Monad m => Semigroup err => err -> Validator m err i Unit
end err = Tokenized.liftUntokenized $ Validator.liftFnEither case _ of
  Nothing → Right unit
  Just _ → Left err
