module Polyform.Type.Row (class Cons') where

import Prim.Row (class Cons, class Lacks) as Row

class (Row.Lacks l r, Row.Cons l o r r') ⇐ Cons' (l ∷ Symbol) o (r ∷ # Type) (r' ∷ # Type)

instance cons' ∷ (Row.Lacks l r, Row.Cons l o r r') ⇒ Cons' l o r r'

