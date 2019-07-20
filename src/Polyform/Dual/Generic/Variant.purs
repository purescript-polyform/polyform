module Polyform.Dual.Generic.Variant where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative ((<|>))
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, case_, inj, on)
import Data.Variant (expand) as Variant
import Polyform.Dual (Dual(..), DualD(..), dual)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.Row (class Union)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record (delete, get) as Record
import Type.Prelude (class IsSymbol, RLProxy(..))

class GDualVariant p i (dl ∷ RowList) (d ∷ # Type) (v ∷ # Type) | dl → d p i v where
  gDualV
    ∷ Alt (p i)
    ⇒ Functor (p i)
    ⇒ RLProxy dl
    → (∀ a s. IsSymbol s ⇒ SProxy s → Dual p i a → Dual p i a)
    → { | d }
    → Dual p i (Variant v)

instance gDualVariantLast ∷
  ( IsSymbol s
  , Row.Cons s a () v
  , Row.Cons s (Dual p i a) () d
  ) ⇒ GDualVariant p i (Cons s (Dual p i a) Nil) d v where
  gDualV _ pre duals = d
    where
      _s = SProxy ∷ SProxy s

      Dual (DualD fieldPrs fieldSer) = pre _s (Record.get _s duals)
      prs = (inj _s <$> fieldPrs)
      ser = case_ # on _s fieldSer

      d = dual prs ser

else instance gDualVariantCons ∷
  ( IsSymbol s
  , Row.Cons s a () x
  , Union vt x v
  , Row.Cons s a vt v
  , Row.Lacks s dt
  , Row.Cons s (Dual p i a) dt d
  , GDualVariant p i dlt dt vt
  ) ⇒ GDualVariant p i (Cons s (Dual p i a) dlt) d v where
  gDualV _ pre duals = d
    where
      _s = SProxy ∷ SProxy s

      duals' ∷ { | dt }
      duals' = Record.delete _s duals
      Dual (DualD prs ser) = gDualV (RLProxy ∷ RLProxy dlt) pre duals'
      Dual (DualD fieldPrs fieldSer) = pre _s (Record.get _s duals)

      prs' = (inj _s <$> fieldPrs) <|> (Variant.expand <$> prs)
      ser' = ser # on _s fieldSer

      d = dual prs' ser'

variant
  ∷ ∀ d dl p i v
  . RowToList d dl
  ⇒ Alt (p i)
  ⇒ Functor (p i)
  ⇒ GDualVariant p i dl d v
  ⇒ (∀ a s. IsSymbol s ⇒ SProxy s → Dual p i a → Dual p i a)
  → { | d }
  → Dual p i (Variant v)
variant pre duals =
  gDualV (RLProxy ∷ RLProxy dl) pre duals


