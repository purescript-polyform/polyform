module Polyform.Validator.Dual
  ( Dual
  , DualD
  , check
  , checkM
  , iso
  , lmapDual
  , lmapDualD
  , liftEither
  , lmapM
  , newtypeIso
  , hoist
  , invalidate
  , smartNewtypeIso
  , runSerializer
  , runValidator
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Validation.Semigroup (V)
import Polyform.Dual (Dual(..), DualD(..), dual, dual', parser, serializer) as Dual
import Polyform.Dual (dual) as Polyform.Dual
import Polyform.Validator (Validator, lmapValidator)
import Polyform.Validator (liftFn, liftFnEither) as Validators
import Polyform.Validator as Validator

type Dual m e i o = Dual.Dual (Validator m e) m i o

type DualD m e i o' o = Dual.DualD (Validator m e) m i o' o

runValidator ∷ ∀ e i o m. Monad m ⇒ Dual.Dual (Validator m e) m i o → (i → m (V e o))
runValidator = Validator.runValidator <<< Dual.parser

runSerializer ∷ ∀ e i o m. Applicative m ⇒ Dual m e i o → (o → m i)
runSerializer = Dual.serializer

hoist ∷ ∀ e i o m m'. Functor m ⇒ (m ~> m') → Dual m e i o → Dual m' e i o
hoist nt (Dual.Dual (Dual.DualD prs ser)) = Dual.dual prs' ser'
  where
    ser' = map nt ser
    prs' = Validator.hoist nt prs

iso ∷ ∀ e i m o. Semigroup e ⇒ Applicative m ⇒ (i → o) → (o → i) → Dual m e i o
iso p s = Dual.dual (Validator.liftFn p) (s >>> pure)

-- | Using not so smart constructor so succeed all the time
newtypeIso ∷ ∀ a e m n. Monad m ⇒ Semigroup e ⇒ Newtype n a ⇒ Dual m e a n
newtypeIso = iso wrap unwrap

-- | Using smart constructor so possibly fail
smartNewtypeIso ∷ ∀ a e m n. Monad m ⇒ Semigroup e ⇒ Newtype n a ⇒ (a → Maybe n) → (a → e) → Dual m e a n
smartNewtypeIso constructor e = Dual.dual
  (Validator.liftFnMaybe e constructor)
  (unwrap >>> pure)

invalidate ∷ ∀ e i m. Applicative m ⇒ (i → e) → Dual m e i i
invalidate e = Dual.dual (Validator.invalidate e) (identity >>> pure)

check ∷ ∀ e i m. Applicative m ⇒ Semigroup e ⇒ (i → e) → (i → Boolean) → Dual m e i i
check e c = Dual.dual' (Validator.check e c)

checkM ∷ ∀ e i m. Monad m ⇒ Semigroup e ⇒ (i → e) → (i → m Boolean) → Dual m e i i
checkM e c = Dual.dual' (Validator.checkM e c)

lmapDualD ∷ ∀ e e' i m o o'. Monad m ⇒ (e → e') → DualD m e i o' o → DualD m e' i o' o
lmapDualD f (Dual.DualD prs ser) = Dual.DualD (lmapValidator f prs) ser

lmapDual ∷ ∀ e e' i m o. Monad m ⇒ (e → e') → Dual m e i o → Dual m e' i o
lmapDual f (Dual.Dual dualD) = Dual.Dual (lmapDualD f dualD)

lmapM ∷ ∀ e e' i m o. Monad m ⇒ (e → m e') → Dual m e i o → Dual m e' i o
lmapM f (Dual.Dual (Dual.DualD prs ser)) = Dual.dual (Validator.lmapM f prs) ser

liftEither ∷ ∀ e m o. Applicative m ⇒ Semigroup e ⇒ Dual m e (Either e o) o
liftEither = Polyform.Dual.dual Validator.liftEither (Right >>> pure)

