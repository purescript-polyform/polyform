module Polyform.Type.Row where

import Prim.Row (class Cons, class Lacks) as Row
import Type.Prelude (class IsSymbol)

class (IsSymbol l, Row.Lacks l r, Row.Cons l o r r') ⇐ Cons' (l ∷ Symbol) o (r ∷ # Type) (r' ∷ # Type)

instance cons' ∷ (IsSymbol l, Row.Lacks l r, Row.Cons l o r r') ⇒ Cons' l o r r'

