module Polyform.Field.Interpret where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Profunctor.Star (Star(..))
import Data.Variant (Variant, inj)
import Polyform.Field.Validation (Validation(..))
import Run (FProxy, Run)
import Run (FProxy(..), VariantF, case_, on, Run)
import Run as Run
import Type.Prelude (class IsSymbol, SProxy(..))

data StringF n e q a = StringF n q (Either e String → a)
derive instance functorStringF ∷ Functor (StringF n q e)

type STRING n e q = FProxy (StringF (Variant n) e q)

_string = SProxy :: SProxy "string"

string ∷ ∀ e q eff name names names'
  . RowCons name Unit names names'
  ⇒ IsSymbol name
  ⇒ SProxy name
  → Validation
      (Run (string ∷ (STRING names' e q) | eff)) e q String
string name =
  Validation $ Star \q → ExceptT (Run.lift _string (StringF (inj name unit) q id))


data OptStringF n q e a = OptStringF (SProxy n) q (Either e (Maybe String) → a)
derive instance functorOptStringF ∷ Functor (OptStringF n q e)

type OPTSTRING n q e a = FProxy (OptStringF n q e)

_optString = SProxy :: SProxy "optString"

optString
  ∷ ∀ a e eff n q
  . SProxy n
  → Validation
      (Run (optString ∷ OPTSTRING n q e a | eff)) e q (Maybe String)
optString name =
  Validation $ Star \q → ExceptT (Run.lift _optString (OptStringF name q id))


data IntF n e q a = IntF n q (Either e Int → a)
derive instance functorIntF ∷ Functor (IntF n e q)

type INT names e q = FProxy (IntF (Variant names) e q)

_int = SProxy :: SProxy "int"

int
  ∷ ∀ e q eff name names names'
  . RowCons name Unit names names'
  ⇒ IsSymbol name
  ⇒ SProxy name
  → Validation
    (Run ( int :: (INT names' e q) | eff)) e q Int
int name =
  Validation $ Star \q → ExceptT (Run.lift _int (IntF (inj name unit) q id))


data OptIntF n q e a = OptIntF (SProxy n) q (Either e (Maybe Int) → a)
derive instance functorOptIntF ∷ Functor (OptIntF n q e)

type OPTINT n q e a = FProxy (OptIntF n q e)

_optInt = SProxy :: SProxy "optInt"

optInt
  ∷ ∀ a e eff n q
  . SProxy n
  → Validation
      (Run (optInt ∷ OPTINT n q e a | eff)) e q (Maybe Int)
optInt name =
  Validation $ Star \q → ExceptT (Run.lift _optInt (OptIntF name q id))

