module Polyform.Dual.Variant where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Symbol (SProxy)
import Data.Variant (Variant, inj)
import Data.Variant (expand, on) as Variant
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafeCrashWith)
import Polyform.Dual (Dual(..), DualD(..), dual)
import Prim.Row (class Cons) as Row
import Prim.Row (class Union)
import Type.Prelude (class IsSymbol)

on ∷ ∀ a i l lr p r r'
  . Union r lr r'
  ⇒ IsSymbol l
  ⇒ Row.Cons l a () lr
  ⇒ Row.Cons l a r r'
  ⇒ Alt (p i)
  ⇒ (∀ a' s. SProxy s → Dual p i a' → Dual p i a')
  → SProxy l
  → Dual p i a
  → Dual p i (Variant r)
  → Dual p i (Variant r')
on pre label d (Dual (DualD restPrs restSer)) =
  let
    Dual (DualD prs ser) = pre label d
  in
    dual
      (inj label <$> prs <|> (Variant.expand <$> restPrs))
      (restSer # Variant.on label ser)

case_ ∷ ∀ i p. Applicative (p i) ⇒ Dual p i (Variant ())
case_ = dual prs ser
  where
    prs = unsafeCrashWith ("Duals.Json.case_: trying to parse empty Variant from: ")
    ser i = unsafeCrashWith ("Duals.Jso.case_: trying to serialize empty Variant: " <> unsafeStringify i)


