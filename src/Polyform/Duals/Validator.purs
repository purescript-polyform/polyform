module Polyform.Duals.Validator where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Validation.Semigroup (V)
import Polyform.Dual (Dual(..), DualD(..), dual, parser, serializer) as Dual
import Polyform.Validator (Validator, lmapValidator)
import Polyform.Validator as Validator
import Polyform.Validator.Par as Validator.Par

type Dual m e i o = Dual.Dual (Validator m e) i o

type DualD m e i o' o = Dual.DualD (Validator m e) i o o'

runValidator ∷ ∀ e i o m. Monad m ⇒ Dual.Dual (Validator m e) i o → (i → m (V e o))
runValidator = Validator.runValidator <<< Dual.parser

-- | Dirty hack to simplify type inference for polymorphic duals
runSerializer ∷ ∀ e i o m. Applicative m ⇒ Dual.Dual (Validator m e) i o → (o → i)
runSerializer = Dual.serializer

runSerializerM ∷ ∀ e i o m. Applicative m ⇒ Dual.Dual (Validator m e) i o → (o → m i)
runSerializerM = map pure <<< Dual.serializer

newtypeDual ∷ ∀ a e m n. Monad m ⇒ Semigroup e ⇒ Newtype n a ⇒ Dual m e a n
newtypeDual = Dual.dual (Validator.hoistFn wrap) unwrap

hoist ∷ ∀ e i o m m'. Functor m ⇒ (m ~> m') → Dual m e i o → Dual m' e i o
hoist nt (Dual.Dual (Dual.DualD prs ser)) = Dual.dual prs' ser
  where
    prs' = Validator.hoist nt prs

newtype Par m r a b = Dual (Validator.Par.Par m r a b)

lmapDual ∷ ∀ e e' i m o. Monad m ⇒ (e → e') → Dual m e i o → Dual m e' i o
lmapDual f (Dual.Dual (Dual.DualD prs ser)) = Dual.dual (lmapValidator f prs) ser
