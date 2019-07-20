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

sum ∷ ∀ a m e i rep r
  .  Monad m
  ⇒ Semigroup e
  ⇒ Generic a rep
  ⇒ GDualSum (Validator m e) i rep r
  ⇒ { | r }
  → Dual (Validator m e) i a
sum = Dual.Generic.sum

variant ∷ ∀ e i d dl m v
  .  Monad m
  ⇒ RowToList d dl
  ⇒ Semigroup e
  ⇒ GDualVariant (Validator m e) i dl d v
  ⇒ (∀ a s. IsSymbol s ⇒ SProxy s → Dual (Validator m e) i a → Dual (Validator m e) i a)
  → { | d }
  → Dual (Validator m e) i (Variant v)
variant pre = Dual.Generic.variant pre

