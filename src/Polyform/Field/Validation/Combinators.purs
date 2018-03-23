module Polyform.Field.Validation.Combinators where

import Prelude

import Data.Array (uncons)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.NonEmpty (NonEmpty(..))
import Data.Variant (Variant, inj)
import Polyform.Validation (V(..), Validation, hoistFnV)
import Type.Prelude (class IsSymbol, SProxy(SProxy))

-- | These helpers seems rather useful only
-- | in case of field validation scenarios
-- | so they are here.
check
  ∷ ∀ a e m
  . Monad m
  ⇒ Monoid e
  ⇒ (a → e)
  → (a → Boolean)
  → Validation m e a a
check singleton f = hoistFnV $ \i →
  let
    e = singleton i
  in
    if f i
      then Valid e i
      else Invalid e

checkAndTag
  ∷ ∀ a e err e' m n
  . Monad m
  ⇒ RowCons n a e' e
  ⇒ IsSymbol n
  ⇒ Monoid err
  ⇒ (Variant e → err)
  → SProxy n
  → (a -> Boolean)
  → Validation m err a a
checkAndTag singleton n c = check (inj n >>> singleton) c

_scalar = (SProxy ∷ SProxy "scalar")

scalar
  ∷ ∀ a err m r
  . (Monad m)
  ⇒ (Monoid err)
  ⇒ (Variant (scalar ∷ NonEmpty Array a | r) → err)
  → Validation m err (NonEmpty Array a) a
scalar singleton = hoistFnV $ case _ of
  NonEmpty a [] → pure a
  arr → Invalid (singleton (inj _scalar arr))

_required = SProxy ∷ SProxy "required"

required
  ∷ ∀ a err m r
  . Monad m
  ⇒ Monoid err
  ⇒ (Variant (required ∷ Unit | r) → err)
  → Validation m err (Array a) (NonEmpty Array a)
required singleton = hoistFnV $ case _ of
  [] → Invalid (singleton (inj _required unit))
  arr → case uncons arr of
    Nothing → Invalid (singleton (inj _required unit))
    Just { head, tail } → pure (NonEmpty head tail)

_int = SProxy ∷ SProxy "int"

type IntErr e = (int ∷ String | e)

int
  ∷ ∀ m err r
  . Monad m
  ⇒ Monoid err
  ⇒ (Variant (IntErr r) → err)
  → Validation m err String Int -- err String Int
int singleton = hoistFnV (\i → case fromString i of
  Just a → pure a
  Nothing → Invalid $ singleton (inj _int i))

