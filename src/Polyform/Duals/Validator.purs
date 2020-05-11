module Polyform.Duals.Validator where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Validation.Semigroup (V)
import Polyform.Dual (Dual(..), DualD(..), dual, parser, serializer) as Dual
import Polyform.Validator (Validator, lmapValidator)
import Polyform.Validator as Validator
import Polyform.Validator.Par as Validator.Par

type Dual m s e i o = Dual.Dual (Validator m e) s i o

type DualD m s e i o' o = Dual.DualD (Validator m e) s i o' o

runValidator ∷ ∀ e i o m s. Monad m ⇒ Dual.Dual (Validator m e) s i o → (i → m (V e o))
runValidator = Validator.runValidator <<< Dual.parser

-- | Dirty hack to simplify type inference for polymorphic duals
runSerializer ∷ ∀ e i o m s. Applicative m ⇒ Dual.Dual (Validator m e) s i o → (o → s i)
runSerializer = Dual.serializer

runSerializerM ∷ ∀ e i o m. Applicative m ⇒ Dual.Dual (Validator m e) m i o → (o → m i)
runSerializerM = Dual.serializer

newtypeDual ∷ ∀ a e m n s. Monad m ⇒ Semigroup e ⇒ Applicative s ⇒ Newtype n a ⇒ Dual m s e a n
newtypeDual = Dual.dual (Validator.hoistFn wrap) (unwrap >>> pure)

hoistParser ∷ ∀ e i o m m' s. Functor m ⇒ (m ~> m') → Dual m s e i o → Dual m' s e i o
hoistParser nt (Dual.Dual (Dual.DualD prs ser)) = Dual.dual prs' ser
  where
    prs' = Validator.hoist nt prs

hoistSerializer ∷ ∀ e i o m s s'. Functor m ⇒ (s ~> s') → Dual m s e i o → Dual m s' e i o
hoistSerializer nt (Dual.Dual (Dual.DualD prs ser)) = Dual.dual prs ser'
  where
    ser' = map nt ser

hoist ∷ ∀ e i o m m' s s'. Functor m ⇒ (m ~> m') → (s ~> s') → Dual m s e i o → Dual m' s' e i o
hoist mnt snt = hoistParser mnt <<< hoistSerializer snt

newtype Par m r a b = Dual (Validator.Par.Par m r a b)

lmapDual ∷ ∀ e e' i m o s. Monad m ⇒ (e → e') → Dual m s e i o → Dual m s e' i o
lmapDual f (Dual.Dual (Dual.DualD prs ser)) = Dual.dual (lmapValidator f prs) ser
