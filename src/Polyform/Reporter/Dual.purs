module Polyform.Reporter.Dual
  ( Dual
  , DualD
  , changeSerializerWith
  , fromValidatorDualWith'
  , hoist
  , hoistParser
  , hoistSerializer
  , liftValidatorDual
  , liftValidatorDualWith
  , runReporter
  , runSerializer
  , runSerializerM
  )
  where

import Prelude

import Data.Tuple (Tuple(..))
import Polyform.Dual (Dual(..), DualD(..), dual, parser, serializer) as Dual
import Polyform.Reporter (R, Reporter, hoist, liftValidator, liftValidatorWith, liftValidatorWith', runReporter) as Reporter
import Polyform.Validator (Validator) as Validator

type Dual m s r i o = Dual.Dual (Reporter.Reporter m r) s i o

type DualD m s r i o' o = Dual.DualD (Reporter.Reporter m r) s i o' o

runReporter ∷ ∀ r i m o s. Dual.Dual (Reporter.Reporter m r) s i o → (i → m (Reporter.R r o))
runReporter = Reporter.runReporter <<< Dual.parser

runSerializer ∷ ∀ e i o m s. Applicative m ⇒ Dual m s e i o → (o → s i)
runSerializer = Dual.serializer

-- | A dirty hack to simplify type inference for polymorphic duals
runSerializerM ∷ ∀ e i o m. Applicative m ⇒ Dual m m e i o → (o → m i)
runSerializerM = Dual.serializer

hoistParser ∷ ∀ e i o m m' s. Functor m ⇒ (m ~> m') → Dual m s e i o → Dual m' s e i o
hoistParser nt (Dual.Dual (Dual.DualD prs ser)) = Dual.dual prs' ser
  where
    prs' = Reporter.hoist nt prs

hoistSerializer ∷ ∀ e i o m s s'. Functor m ⇒ (s ~> s') → Dual m s e i o → Dual m s' e i o
hoistSerializer nt (Dual.Dual (Dual.DualD prs ser)) = Dual.dual prs ser'
  where
    ser' = map nt ser

hoist ∷ ∀ e i o m m' s s'. Functor m ⇒ (m ~> m') → (s ~> s') → Dual m s e i o → Dual m' s' e i o
hoist mnt snt = hoistParser mnt <<< hoistSerializer snt

liftValidatorDual ∷ ∀ i m r s
  . Functor m
  ⇒ Monoid r
  ⇒ Dual.Dual (Validator.Validator m r) s i
  ~> Dual m s r i
liftValidatorDual d = Dual.dual
  (Reporter.liftValidator $ Dual.parser d)
  (Dual.serializer d)

changeSerializerWith ∷ ∀ i m o r s s'
  . Functor s
  ⇒ (s (Tuple i o) -> s' i)
  → Dual m s r i o
  → Dual m s' r i o
changeSerializerWith fo d = Dual.dual (Dual.parser d) ser
  where
    ser o =
      let
         s = Dual.serializer d o
      in
        fo (flip Tuple o <$> s)

liftValidatorDualWith ∷ ∀ e i m o r s
  . Functor m
  ⇒ (Tuple i e → r)
  → (Tuple i o → r)
  → Dual.Dual (Validator.Validator m e) s i o
  → Dual m s r i o
liftValidatorDualWith fe fo d = Dual.dual
  (Reporter.liftValidatorWith fe fo $ Dual.parser d)
  (Dual.serializer d)

fromValidatorDualWith' ∷ ∀ i m o r s
  . Functor m
  ⇒ (Tuple i o → r)
  → Dual.Dual (Validator.Validator m r) s i o
  → Dual m s r i o
fromValidatorDualWith' fo d = Dual.dual
  (Reporter.liftValidatorWith' fo $ Dual.parser d)
  (Dual.serializer d)

-- newtype Par m r a b = Dual (Reporter.Par.Par m r a b)
