module Polyform.Dual.Generic.Sum where

import Prelude hiding (unit)

import Control.Alt (class Alt)
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product, Sum(..), from, to)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (class Profunctor, dimap)
import Data.Symbol (SProxy(..))
import Polyform.Dual (Dual(..), DualD(..), dual)
import Prelude (unit) as Prelude
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Type.Prelude (class IsSymbol)

class GDualSum p i rep (r ∷ # Type) | rep → r p i where
  gDual
    ∷ Functor (p i)
    ⇒ Alt (p i)
    ⇒ (∀ a s. IsSymbol s ⇒ SProxy s → Dual p i a → Dual p i a)
    → { | r }
    → Dual p i rep

instance gDualSum ::
  ( GDualSum p i a r
  , GDualSum p i b r
  ) =>
  GDualSum p i (Sum a b) r where
  gDual pre r = dual prs ser
    where
    Dual (DualD prsl serl) = gDual pre r
    Dual (DualD prsr serr) = gDual pre r
    ser = case _ of
      Inl a → serl a
      Inr b → serr b
    prs = Inl <$> prsl <|> Inr <$> prsr

instance gDualConstructor ::
  ( IsSymbol sym
  , Row.Cons sym (Dual p i a) rx r
  , GDualCtr p i a b
  ) =>
  GDualSum p i (Constructor sym b) r where
    gDual pre r = dual prs' ser'
      where
      _s = SProxy ∷ SProxy sym
      Dual (DualD prs ser) = pre _s $
        (gDualCtr ∷ Dual p i a → Dual p i b) (Record.get _s r)
      ser' (Constructor a) = ser a
      prs' = Constructor <$> prs

class GDualCtr p i o o' | o → o' where
  gDualCtr ∷ Functor (p i) ⇒ Dual p i o → Dual p i o'

instance gDualProduct ::
  GDualCtr p i (Product a b) (Product a b) where
  gDualCtr = identity
else
instance gDualNoArguments ::
  GDualCtr p i NoArguments NoArguments where
  gDualCtr = identity
else
instance gDualArgument ::
  GDualCtr p i (Argument a) (Argument a) where
  gDualCtr = identity
else
instance gDualAll ::
  GDualCtr p i a (Argument a) where
  gDualCtr (Dual (DualD prs ser)) =
    dual (Argument <$> prs) (\(Argument a) → ser a)

sum ∷ ∀ a i p rep r
  . Generic a rep
  ⇒ GDualSum p i rep r
  ⇒ Functor (p i)
  ⇒ Alt (p i)
  ⇒ Profunctor p
  ⇒ (∀ x s. IsSymbol s ⇒ SProxy s → Dual p i x → Dual p i x)
  → { | r }
  → Dual p i a
sum pre = wrap <<< dimap from to <<< unwrap <<< gDual pre

noArgs :: ∀ p i. Applicative (p i) ⇒ Monoid i ⇒ Dual p i NoArguments
noArgs = Dual (pure NoArguments)

noArgs' ∷ ∀ p i. Applicative (p i) ⇒ i → Dual p i NoArguments
noArgs' i = dual (pure NoArguments) (const i)

unit ∷ ∀ p i. Applicative (p i) ⇒ Monoid i ⇒ Dual p i Unit
unit = Dual (pure Prelude.unit)

unit' ∷ ∀ p i. Applicative (p i) ⇒ i → Dual p i Unit
unit' i = dual (pure Prelude.unit) (const i)
