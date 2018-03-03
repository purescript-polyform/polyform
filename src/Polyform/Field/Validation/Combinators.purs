module Polyform.Field.Validation.Combinators where

import Prelude

import Data.Either (note)
import Data.Int (fromString)
import Data.Variant (Variant)
import Polyform.Field.Validation (Validation, hoistEither, tag)
import Type.Prelude (SProxy(..))

_int ∷ SProxy "int"
_int = SProxy

int
  ∷ ∀ m e
  . Monad m
  ⇒ Validation m (Variant (int ∷ String | e)) String Int
int = tag _int $ hoistEither $ note <*> fromString

