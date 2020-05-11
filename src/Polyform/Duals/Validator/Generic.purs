module Polyform.Duals.Validator.Generic where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Polyform.Dual (Dual)
import Polyform.Dual.Generic (class GDualSum, class GDualVariant)
import Polyform.Dual.Generic (sum, variant) as Dual.Generic
import Polyform.Validator (Validator)
import Prim.RowList (class RowToList)

sum ∷ ∀ a m e i rep r s
  .  Monad m
  ⇒ Semigroup e
  ⇒ Generic a rep
  ⇒ GDualSum (Validator m e) s i rep r
  ⇒ (∀ x l. IsSymbol l ⇒ SProxy l → Dual (Validator m e) s i x → Dual (Validator m e) s i x)
  → { | r }
  → Dual (Validator m e) s i a
sum = Dual.Generic.sum

variant ∷ ∀ e i d dl m s v
  . Monad m
  ⇒ RowToList d dl
  ⇒ Semigroup e
  ⇒ GDualVariant (Validator m e) s i dl d v
  ⇒ (∀ a l. IsSymbol l ⇒ SProxy l → Dual (Validator m e) s i a → Dual (Validator m e) s i a)
  → { | d }
  → Dual (Validator m e) s i (Variant v)
variant = Dual.Generic.variant

