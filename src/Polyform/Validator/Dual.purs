module Polyform.Validator.Dual
  ( Dual
  , DualD
  , check
  , checkM
  , fromNewtype
  , hoist
  , hoistSerializer
  , hoistValidator
  , invalidate
  , liftSmartConstructor
  , liftFns
  , lmapDual
  , runSerializer
  , runSerializerM
  , runValidator
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Validation.Semigroup (V)
import Polyform.Dual (Dual(..), DualD(..), dual, dual', parser, serializer) as Dual
import Polyform.Validator (Validator, lmapValidator)
import Polyform.Validator as Validator

type Dual m s e i o = Dual.Dual (Validator m e) s i o

type DualD m s e i o' o = Dual.DualD (Validator m e) s i o' o

runValidator ∷ ∀ e i o m s. Monad m ⇒ Dual.Dual (Validator m e) s i o → (i → m (V e o))
runValidator = Validator.runValidator <<< Dual.parser

runSerializer ∷ ∀ e i o m s. Applicative m ⇒ Dual m s e i o → (o → s i)
runSerializer = Dual.serializer

-- | A dirty hack to simplify type inference for polymorphic duals
runSerializerM ∷ ∀ e i o m. Applicative m ⇒ Dual.Dual (Validator m e) m i o → (o → m i)
runSerializerM = Dual.serializer

-- | I'm not sure if this is helpful at all
hoistValidator ∷ ∀ e i m m' s. Functor m ⇒ m ~> m' → Dual m s e i ~> Dual m' s e i
hoistValidator nt (Dual.Dual (Dual.DualD prs ser)) = Dual.dual prs' ser
  where
    prs' = Validator.hoist nt prs

hoistSerializer ∷ ∀ e i o m s s'. Functor m ⇒ s ~> s' → Dual m s e i o → Dual m s' e i o
hoistSerializer nt (Dual.Dual (Dual.DualD prs ser)) = Dual.dual prs ser'
  where
    ser' = map nt ser

hoist ∷ ∀ e i o m m' s s'. Functor m ⇒ (m ~> m') → (s ~> s') → Dual m s e i o → Dual m' s' e i o
hoist mnt snt = hoistValidator mnt <<< hoistSerializer snt

liftFns ∷ ∀ e i m o s. Semigroup e ⇒ Applicative m ⇒ Applicative s ⇒ (i → o) → (o → i) → Dual m s e i o
liftFns p s = Dual.dual (Validator.liftFn p) (s >>> pure)

invalidate ∷ ∀ e i m s. Applicative m ⇒ Applicative s ⇒ (i → e) → Dual m s e i i
invalidate e = Dual.dual (Validator.invalidate e) (identity >>> pure)

-- | Using smart constructor so possibly fail
liftSmartConstructor ∷ ∀ a e m n s. Monad m ⇒ Semigroup e ⇒ Applicative s ⇒ Newtype n a ⇒ (a → Maybe n) → (a → e) → Dual m s e a n
liftSmartConstructor constructor e = Dual.dual
  (Validator.liftFnMaybe e constructor)
  (unwrap >>> pure)

-- | Using not so smart constructor so succeed all the time
fromNewtype ∷ ∀ a e m n s. Monad m ⇒ Semigroup e ⇒ Applicative s ⇒ Newtype n a ⇒ Dual m s e a n
fromNewtype = Dual.dual (Validator.liftFn wrap) (unwrap >>> pure)

check ∷ ∀ e i m s. Applicative m ⇒ Applicative s ⇒ Semigroup e ⇒ (i → e) → (i → Boolean) → Dual m s e i i
check e c = Dual.dual' (Validator.check e c)

checkM ∷ ∀ e i m s. Monad m ⇒ Applicative s ⇒ Semigroup e ⇒ (i → e) → (i → m Boolean) → Dual m s e i i
checkM e c = Dual.dual' (Validator.checkM e c)

lmapDual ∷ ∀ e e' i m o s. Monad m ⇒ (e → e') → Dual m s e i o → Dual m s e' i o
lmapDual f (Dual.Dual (Dual.DualD prs ser)) = Dual.dual (lmapValidator f prs) ser
