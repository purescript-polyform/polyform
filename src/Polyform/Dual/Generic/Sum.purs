module Polyform.Dual.Generic.Sum where

import Prelude hiding (unit)

import Control.Alt (class Alt)
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product, Sum(..), from, to)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (class Profunctor, dimap)
import Polyform.Dual (Dual(..), DualD(..), dual)
import Prelude (unit) as Prelude
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))

class GDualSum :: (Type -> Type -> Type) -> (Type -> Type) -> Type -> Type -> Row Type -> Constraint
class GDualSum p s i rep r | rep → r p i where
  gDual
    ∷ Functor (p i)
    ⇒ Alt (p i)
    ⇒ (∀ a l. IsSymbol l ⇒ Proxy l → Dual p s i a → Dual p s i a)
    → { | r }
    → Dual p s i rep

instance gDualSum ::
  ( GDualSum p s i a r
  , GDualSum p s i b r
  ) =>
  GDualSum p s i (Sum a b) r where
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
  , Row.Cons sym (Dual p s i a) rx r
  , GDualCtr p s i a b
  ) =>
  GDualSum p s i (Constructor sym b) r where
    gDual pre r = dual prs' ser'
      where
      _s = Proxy ∷ Proxy sym
      Dual (DualD prs ser) = pre _s $
        (gDualCtr ∷ Dual p s i a → Dual p s i b) (Record.get _s r)
      ser' (Constructor a) = ser a
      prs' = Constructor <$> prs

class GDualCtr ∷ (Type -> Type -> Type) -> (Type -> Type) -> Type -> Type -> Type -> Constraint
class GDualCtr p s i o o' | o → o' where
  gDualCtr ∷ Functor (p i) ⇒ Dual p s i o → Dual p s i o'

instance gDualProduct ::
  GDualCtr p s i (Product a b) (Product a b) where
  gDualCtr = identity
else
instance gDualNoArguments ::
  GDualCtr p s i NoArguments NoArguments where
  gDualCtr = identity
else
instance gDualArgument ::
  GDualCtr p s i (Argument a) (Argument a) where
  gDualCtr = identity
else
instance gDualAll ::
  GDualCtr p s i a (Argument a) where
  gDualCtr (Dual (DualD prs ser)) =
    dual (Argument <$> prs) (\(Argument a) → ser a)

sum ∷ ∀ a i p rep r s
  . Generic a rep
  ⇒ GDualSum p s i rep r
  ⇒ Functor (p i)
  ⇒ Alt (p i)
  ⇒ Profunctor p
  ⇒ (∀ x l. IsSymbol l ⇒ Proxy l → Dual p s i x → Dual p s i x)
  → { | r }
  → Dual p s i a
sum pre = wrap <<< dimap from to <<< unwrap <<< gDual pre

noArgs :: ∀ i p s. Applicative (p i) ⇒ Applicative s ⇒ Monoid i ⇒ Dual p s i NoArguments
noArgs = Dual (pure NoArguments)

noArgs' ∷ ∀ i p s. Applicative (p i) ⇒ Applicative s ⇒ i → Dual p s i NoArguments
noArgs' i = dual (pure NoArguments) (const $ pure i)

unit ∷ ∀ i p s. Applicative (p i) ⇒ Applicative s ⇒ Monoid i ⇒ Dual p s i Unit
unit = Dual (pure Prelude.unit)

unit' ∷ ∀ i p s. Applicative (p i) ⇒ Applicative s ⇒ i → Dual p s i Unit
unit' i = dual (pure Prelude.unit) (const $ pure i)
