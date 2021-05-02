module Polyform.Reporter.Dual
  ( Dual
  , DualD
  , hoist
  , iso
  , liftValidatorDual
  , liftValidatorDualWith
  , mapReport
  , newtypeIso
  , runReporter
  , runSerializer
  )
  where

import Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (Writer, mapWriter, runWriter, tell)
import Data.Newtype (class Newtype, un, unwrap, wrap)
import Data.Profunctor (lcmap) as Profunctor
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (first) as Profunctor.Strong
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Polyform.Dual (Dual(..), DualD(..), dual, parser, serializer) as Dual
import Polyform.Reporter (R)
import Polyform.Reporter (Reporter, hoist, liftValidator, liftValidatorWith, lmapM, runReporter) as Reporter
import Polyform.Validator.Dual (Dual, iso) as Validator.Dual

type Dual m r = Dual.Dual (Reporter.Reporter m r) (Writer r)

type DualD m r = Dual.DualD (Reporter.Reporter m r) (Writer r)

runReporter ∷ ∀ r i m o. Dual m r i o → (i → m (R r o))
runReporter = Reporter.runReporter <<< Dual.parser

runSerializer ∷ ∀ i o m r. Applicative m ⇒ Dual m r i o → (o → i /\ r)
runSerializer = map runWriter <<< Dual.serializer

hoist ∷ ∀ e i o m m'. Functor m ⇒ (m ~> m') → Dual m e i o → Dual m' e i o
hoist nt (Dual.Dual (Dual.DualD reporter ser)) = Dual.dual reporter' ser
  where
    reporter' = Reporter.hoist nt reporter

liftValidatorDual ∷ ∀ i m r
  . Monad m
  ⇒ Monoid r
  ⇒ Validator.Dual.Dual m r i ~> Dual m r i
liftValidatorDual d = Dual.dual
  (Reporter.liftValidator $ Dual.parser d)
  (map lift (Dual.serializer d))

liftValidatorDualWith ∷ ∀ e i m o r
  . Monad m
  ⇒ Monoid r
  ⇒ (Tuple i e → r)
  → (Tuple i o → r)
  → Validator.Dual.Dual m e i o
  → Dual m r i o
liftValidatorDualWith fe fo d = Dual.dual
  (Reporter.liftValidatorWith fe fo $ Dual.parser d)
  ser
  where
    ser ∷ o → Writer r i
    ser = attachOutput (Dual.serializer d) >>> lift >=> \t@(Tuple i o) → do
        let
          r = fo t
        tell r
        pure i

    attachOutput :: ∀ m'. Applicative m' ⇒ (o -> m' i) -> o -> m' (i /\ o)
    attachOutput
      = un Star
      <<< Profunctor.lcmap (\o → Tuple o o)
      <<< Profunctor.Strong.first
      <<< Star

mapReport ∷ ∀ i m o r r'. Monad m ⇒ (r → r') → Dual m r i o → Dual m r' i o
mapReport f (Dual.Dual (Dual.DualD reporter ser)) = Dual.dual reporter' ser'
  where
    reporter' = Reporter.lmapM (f >>> pure) reporter

    ser' = mapWriter (map f) <$> ser

iso ∷ ∀ e i m o. Monoid e ⇒ Monad m ⇒ (i → o) → (o → i) → Dual m e i o
iso p s = liftValidatorDual (Validator.Dual.iso p s)

newtypeIso ∷ ∀ a e m n. Monad m ⇒ Monoid e ⇒ Newtype n a ⇒ Dual m e a n
newtypeIso = iso wrap unwrap
