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

class GDualVariant :: (Type -> Type -> Type) -> (Type -> Type) -> Type -> RowList Type -> Row Type -> Row Type -> Constraint
class GDualVariant p s i dl d v | dl → d p s i v where
  gDualV
    ∷ Alt (p i)
    ⇒ Functor (p i)
    ⇒ RLProxy dl
    → (∀ a l. IsSymbol l ⇒ SProxy l → Dual p s i a → Dual p s i a)
    → { | d }
    → Dual p s i (Variant v)

instance gDualVariantLast ∷
  ( IsSymbol l
  , Row.Cons l a () v
  , Row.Cons l (Dual p s i a) () d
  ) ⇒ GDualVariant p s i (Cons l (Dual p s i a) Nil) d v where
  gDualV _ pre duals = d
    where
      _l = SProxy ∷ SProxy l

      Dual (DualD fieldPrs fieldSer) = pre _l (Record.get _l duals)
      prs = (inj _l <$> fieldPrs)
      ser = case_ # on _l fieldSer

      d = dual prs ser

else instance gDualVariantCons ∷
  ( IsSymbol l
  , Row.Cons l a () x
  , Union vt x v
  , Row.Cons l a vt v
  , Row.Lacks l dt
  , Row.Cons l (Dual p s i a) dt d
  , GDualVariant p s i dlt dt vt
  ) ⇒ GDualVariant p s i (Cons l (Dual p s i a) dlt) d v where
  gDualV _ pre duals = d
    where
      _l = SProxy ∷ SProxy l

      duals' ∷ { | dt }
      duals' = Record.delete _l duals
      Dual (DualD prs ser) = gDualV (RLProxy ∷ RLProxy dlt) pre duals'
      Dual (DualD fieldPrs fieldSer) = pre _l (Record.get _l duals)

      prs' = (inj _l <$> fieldPrs) <|> (Variant.expand <$> prs)
      ser' = ser # on _l fieldSer

      d = dual prs' ser'

variant
  ∷ ∀ d dl p i v s
  . RowToList d dl
  ⇒ Alt (p i)
  ⇒ Functor (p i)
  ⇒ GDualVariant p s i dl d v
  ⇒ (∀ a l. IsSymbol l ⇒ SProxy l → Dual p s i a → Dual p s i a)
  → { | d }
  → Dual p s i (Variant v)
variant pre duals =
  gDualV (RLProxy ∷ RLProxy dl) pre duals


