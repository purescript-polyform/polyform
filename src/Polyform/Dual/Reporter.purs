module Polyform.Dual.Reporter where

import Prelude

import Polyform.Dual (Dual)
import Polyform.Dual (dual, parser, serializer) as Dual
import Polyform.Reporter (R, Reporter, hoistValidator, hoistValidatorWith, hoistValidatorWith') as Reporter
import Polyform.Reporter (runReporter) as Reproter
import Polyform.Reporter.Par as Reporter.Par
import Polyform.Validator (Validator) as Validator

type Reporter m e i o = Dual (Reporter.Reporter m e) i o

newtype Par m r a b = Dual (Reporter.Par.Par m r a b)

runReporter ∷ ∀ r i m o. Dual (Reporter.Reporter m r) i o → (i → m (Reporter.R r o))
runReporter = Reproter.runReporter <<< Dual.parser

-- | Dirty hack to simplify type inference for polymorphic duals
runSerializer ∷ ∀ e i o m. Applicative m ⇒ Dual (Reporter.Reporter m e) i o → (o → m i)
runSerializer = map pure <<< Dual.serializer

hoistValidatorWith ∷ ∀ e i m o r
  . Functor m
  ⇒ (e → r)
  → (o → r)
  → Dual (Validator.Validator m e) i o
  → Dual (Reporter.Reporter m r) i o
hoistValidatorWith fe fo vDual = Dual.dual
  { parser:
      Reporter.hoistValidatorWith fe fo $ Dual.parser vDual
  , serializer: Dual.serializer vDual
  }

hoistValidatorWith' ∷ ∀ i m o r
  . Functor m
  ⇒ (o → r)
  → Dual (Validator.Validator m r) i o
  → Dual (Reporter.Reporter m r) i o
hoistValidatorWith' fo vDual = Dual.dual
  { parser:
      Reporter.hoistValidatorWith' fo $ Dual.parser vDual
  , serializer: Dual.serializer vDual
  }

hoistValidator ∷ ∀ i m o r
  . Functor m
  ⇒ Monoid r
  ⇒ Dual (Validator.Validator m r) i o
  → Dual (Reporter.Reporter m r) i o
hoistValidator vDual = Dual.dual
  { parser: Reporter.hoistValidator $ Dual.parser vDual
  , serializer: Dual.serializer vDual
  }

