module Polyform.Input.Interpret where

import Prelude

import Data.Either (either, note)
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (right)
import Polyform.Form.Component (Component, fromField)
import Polyform.Input.Interpret.Validation (INT, OptIntF, OptStringF, STRING, int, optInt, optString, string)
import Polyform.Validation (V, Validation)
import Run (FProxy, Run)
import Type.Prelude (class IsSymbol, SProxy)
import Prim.Row (class Cons)

intForm
  ∷ ∀ attrs e eff form q n ns ns' v
  . Monoid e
  ⇒ Cons n Unit ns ns'
  ⇒ IsSymbol n
  ⇒ ({ value ∷ V e v, name ∷ SProxy n | attrs } -> form)
  → { value :: V e v , name :: SProxy n | attrs }
  → Validation
      (Run ( int ∷ (INT ns' e q) | eff))
      e
      Int
      v
  → Component
      (Run ( int ∷ (INT ns' e q) | eff))
      form
      q
      v
intForm singleton field validation =
  fromField singleton field $ int field.name >>> validation

optIntForm
  ∷ ∀ attrs e eff form q n v
  . Monoid e
  ⇒ ({ value ∷ V e (Maybe v), name ∷ SProxy n | attrs } -> form)
  → { value :: V e (Maybe v), name :: SProxy n | attrs }
  → Validation
      (Run ( optInt ∷ FProxy (OptIntF n q e) | eff))
      e
      Int
      v
  → Component
      (Run ( optInt ∷ FProxy (OptIntF n q e) | eff))
      form
      q
      (Maybe v)
optIntForm singleton field validation =
  fromField singleton field validation'
 where
  validation'
    = optInt field.name
    >>> dimap (note Nothing) (either identity Just) (right validation)

stringForm
  ∷ ∀ attrs e eff form q n ns ns' v
  . Cons n Unit ns ns'
  ⇒ IsSymbol n
  ⇒ Semigroup e
  ⇒ ({ value ∷ V e v, name ∷ SProxy n | attrs } -> form)
  → { value :: V e v , name :: SProxy n | attrs }
  → Validation
      (Run ( string ∷ (STRING ns' e q) | eff))
      e
      String
      v
  → Component
      (Run ( string ∷ (STRING ns' e q) | eff))
      form
      q
      v
stringForm singleton field validation =
  fromField singleton field $ string field.name >>> validation

optStringForm
  ∷ ∀ attrs e eff form q n v
  . Monoid e
  ⇒ ({ value ∷ V e (Maybe v), name ∷ SProxy n | attrs } -> form)
  → { value :: V e (Maybe v), name :: SProxy n | attrs }
  → Validation
      (Run ( optString ∷ FProxy (OptStringF n q e) | eff))
      e
      String
      v
  → Component
      (Run ( optString ∷ FProxy (OptStringF n q e) | eff))
      form
      q
      (Maybe v)
optStringForm singleton field validation =
  fromField singleton field validation'
 where
  validation'
    = optString field.name
    >>> dimap (note Nothing) (either identity Just) (right validation)
