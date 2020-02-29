module Polyform.Dual.Variant where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Symbol (SProxy)
import Data.Variant (Variant, inj)
import Data.Variant (on) as Variant
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafeCrashWith)
import Polyform.Dual (Dual(..), DualD(..), dual)
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol)
import Unsafe.Coerce (unsafeCoerce)

-- |`prefix` can be used to encode tagging of a given option.
-- | Please take a look at `Polyform.Duals.Json.on` example
-- | from `polyform-validators`.
on ∷ ∀ a i l p r r'
  . IsSymbol l
  ⇒ Row.Cons l a r r'
  ⇒ Alt (p i)
  ⇒ (∀ a' s. IsSymbol s ⇒ SProxy s → Dual p i a' → Dual p i a')
  → SProxy l
  → Dual p i a
  → Dual p i (Variant r)
  → Dual p i (Variant r')
on prefix label d (Dual (DualD restPrs restSer)) =
  let
    Dual (DualD prs ser) = prefix label d

    -- | We want to avoid `Union` constraint which
    -- | made usage of this function really cumbersome.
    expandCase ∷ Row.Cons l a r r' ⇒ Variant r → Variant r'
    expandCase = unsafeCoerce
  in
    dual
      (inj label <$> prs <|> ((expandCase <$> restPrs )))
      (restSer # Variant.on label ser)

case_ ∷ ∀ i p. Applicative (p i) ⇒ Dual p i (Variant ())
case_ = dual prs ser
  where
    prs = unsafeCoerce (\_ → unsafeCrashWith ("Dual.Variant.case_: trying to parse empty Variant"))
    ser i = unsafeCrashWith ("Dual.Variant.case_: serializing empty Variant: " <> unsafeStringify i)


