module Polyform.Reporter.Dual
  ( Dual
  , DualD
  , hoist
  , iso
  , liftValidatorDual
  , liftValidatorDualWith
  , liftValidatorDualWithM
  , lmapM
  , newtypeIso
  , runReporter
  , runSerializer
  )
  where

import Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Monad.Writer.Trans (mapWriterT)
import Data.Newtype (class Newtype, un, unwrap, wrap)
import Data.Profunctor (lcmap) as Profunctor
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (first) as Profunctor.Strong
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Polyform.Dual (Dual(..), DualD(..), dual, parser, serializer) as Dual
import Polyform.Reporter (R)
import Polyform.Reporter (Reporter, hoist, liftValidator, liftValidatorWithM, lmapM, runReporter) as Reporter
import Polyform.Validator.Dual (Dual, iso) as Validator.Dual

type Dual m r i o = Dual.Dual (Reporter.Reporter m r) (WriterT r m) i o

type DualD m r i o' o = Dual.DualD (Reporter.Reporter m r) (WriterT r m) i o' o

runReporter ∷ ∀ r i m o. Dual m r i o → (i → m (R r o))
runReporter = Reporter.runReporter <<< Dual.parser

runSerializer ∷ ∀ i o m r. Applicative m ⇒ Dual m r i o → (o → m (Tuple i r))
runSerializer = map runWriterT <<< Dual.serializer

hoist ∷ ∀ e i o m m'. Functor m ⇒ (m ~> m') → Dual m e i o → Dual m' e i o
hoist nt (Dual.Dual (Dual.DualD reporter ser)) = Dual.dual reporter' ser'
  where
    reporter' = Reporter.hoist nt reporter
    ser' = map (mapWriterT nt) ser

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
liftValidatorDualWith fe fo = liftValidatorDualWithM (pure <<< fe) (pure <<< fo)

liftValidatorDualWithM ∷ ∀ e i m o r
  . Monad m
  ⇒ Monoid r
  ⇒ (Tuple i e → m r)
  → (Tuple i o → m r)
  → Validator.Dual.Dual m e i o
  → Dual m r i o
liftValidatorDualWithM fe fo d = Dual.dual
  (Reporter.liftValidatorWithM fe fo $ Dual.parser d)
  ( attachOutput (Dual.serializer d) >>> lift >=> \t@(Tuple i o) → do
      r ← lift (fo t)
      tell r
      pure i
  )
  where
    attachOutput :: Functor m => (o -> m i) -> o -> m (Tuple i o)
    attachOutput
      = un Star
      <<< Profunctor.lcmap (\o → Tuple o o)
      <<< Profunctor.Strong.first
      <<< Star

lmapM ∷ ∀ i m o r r'. Monad m ⇒ (r → m r') → Dual m r i o → Dual m r' i o
lmapM f (Dual.Dual (Dual.DualD reporter ser)) = Dual.dual reporter' ser'
  where
    reporter' = Reporter.lmapM f reporter

    ser' = mapWriterT (_ >>= f') <$> ser
      where
        -- f' ∷ Tuple i r → m (Tuple i r')
        f' = map f >>> sequence

iso ∷ ∀ e i m o. Monoid e ⇒ Monad m ⇒ (i → o) → (o → i) → Dual m e i o
iso p s = liftValidatorDual (Validator.Dual.iso p s)

newtypeIso ∷ ∀ a e m n. Monad m ⇒ Monoid e ⇒ Newtype n a ⇒ Dual m e a n
newtypeIso = iso wrap unwrap
