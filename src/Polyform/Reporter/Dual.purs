module Polyform.Reporter.Dual
  ( Dual
  , DualD
  -- , changeSerializerWith
  -- , fromValidatorDualWith'
  , hoist
  , liftValidatorDual
  , liftValidatorDualWith
  , runReporter
  , runSerializer
  )
  where

import Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, runWriterT)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Writer.Trans (mapWriterT)
import Data.Tuple (Tuple)
import Polyform.Dual (Dual(..), DualD(..), dual, parser, serializer) as Dual
import Polyform.Reporter (R, Reporter, hoist, liftValidator, runReporter) as Reporter
import Polyform.Validator.Dual (Dual) as Validator.Dual

type Dual m r i o = Dual.Dual (Reporter.Reporter m r) (WriterT r m) i o

type DualD m r i o' o = Dual.DualD (Reporter.Reporter m r) (WriterT r m) i o' o

runReporter ∷ ∀ r i m o. Dual m r i o → (i → m (Reporter.R r o))
runReporter = Reporter.runReporter <<< Dual.parser

runSerializer ∷ ∀ i o m r. Applicative m ⇒ Dual m r i o → (o → m (Tuple i r))
runSerializer = map runWriterT <<< Dual.serializer

hoist ∷ ∀ e i o m m'. Functor m ⇒ (m ~> m') → Dual m e i o → Dual m' e i o
hoist nt (Dual.Dual (Dual.DualD reporter ser)) = Dual.dual reporter' ser'
  where
    reporter' = Reporter.hoist nt reporter
    ser' = map (mapWriterT nt) ser

liftValidatorDual ∷ ∀ i m o r
  . Monad m
  ⇒ Monoid r
  ⇒ Validator.Dual.Dual m r i o
  -> Dual m r i o
liftValidatorDual d = Dual.dual
  (Reporter.liftValidator $ Dual.parser d)
  (map lift (Dual.serializer d))

liftValidatorDualWith ∷ ∀ i m o r
  . Monoid r
  ⇒ Monad m
  ⇒ (i → m r)
  → Validator.Dual.Dual m r i o
  → Dual m r i o
liftValidatorDualWith f d = Dual.dual
  ( Reporter.liftValidator $ Dual.parser d )
  ( Dual.serializer d <#> \mi → do
      i ← lift mi
      lift (f i) >>= tell
      pure i
  )

-- changeSerializerWith ∷ ∀ i m o r s s'
--   . Functor s
--   ⇒ (s (Tuple i o) -> s' i)
--   → Dual m s r i o
--   → Dual m s' r i o
-- changeSerializerWith fo d = Dual.dual (Dual.parser d) ser
--   where
--     ser o =
--       let
--          s = Dual.serializer d o
--       in
--         fo (flip Tuple o <$> s)
-- 
-- fromValidatorDualWith' ∷ ∀ i m o r s
--   . Functor m
--   ⇒ (Tuple i o → r)
--   → Dual.Dual (Validator.Validator m r) s i o
--   → Dual m s r i o
-- fromValidatorDualWith' fo d = Dual.dual
--   (Reporter.liftValidatorWith' fo $ Dual.parser d)
--   (Dual.serializer d)
-- 
-- -- newtype Par m r a b = Dual (Reporter.Par.Par m r a b)
