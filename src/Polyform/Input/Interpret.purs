module Polyform.Input.Interpret where

import Prelude

import Data.Either (Either, either, note)
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (right)
import Polyform.Field as Field
import Polyform.Field.Validation.Interpret (INT, OptIntF, OptStringF, STRING, int, optInt, optString, string)
import Polyform.Form.Component (Component, fromField)
import Run (FProxy, Run)
import Type.Prelude (class IsSymbol, SProxy)

intForm
  ∷ ∀ attrs e eff form q n ns ns' v
  . RowCons n Unit ns ns'
  ⇒ IsSymbol n
  ⇒ ({ value ∷ Either e v, name ∷ SProxy n | attrs } -> form)
  → { value :: Either e v , name :: SProxy n | attrs }
  → Field.Validation
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
  . ({ value ∷ Either e (Maybe v), name ∷ SProxy n | attrs } -> form)
  → { value :: Either e (Maybe v), name :: SProxy n | attrs }
  → Field.Validation
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
    >>> dimap (note Nothing) (either id Just) (right validation)

stringForm
  ∷ ∀ attrs e eff form q n ns ns' v
  . RowCons n Unit ns ns'
  ⇒ IsSymbol n
  ⇒ ({ value ∷ Either e v, name ∷ SProxy n | attrs } -> form)
  → { value :: Either e v , name :: SProxy n | attrs }
  → Field.Validation
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
  . ({ value ∷ Either e (Maybe v), name ∷ SProxy n | attrs } -> form)
  → { value :: Either e (Maybe v), name :: SProxy n | attrs }
  → Field.Validation
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
    >>> dimap (note Nothing) (either id Just) (right validation)
