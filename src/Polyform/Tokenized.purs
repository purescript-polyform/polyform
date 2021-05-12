module Polyform.Tokenized where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor, dimap, rmap)
import Data.Profunctor.Costrong (class Costrong, unfirst, unsecond)
import Data.Profunctor.Strong (class Strong, (***))
import Data.Profunctor.Strong (first, second) as Strong
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\), type (/\))

-- | TODO:
-- | Should we move from `List i` to `Array i /\ Int`
-- | so we can improve error reporting?
newtype Tokenized p i o
  = Tokenized (p (List i /\ Maybe i) (List i /\ o))

instance functorTokenized ∷ (Profunctor p) ⇒ Functor (Tokenized p i) where
  map f (Tokenized p) = Tokenized (rmap (map f) p)

instance applyTokenized ∷ (Profunctor p, Semigroupoid p, Strong p) ⇒ Apply (Tokenized p i) where
  apply (Tokenized pf) (Tokenized pa) =
    Tokenized
      $ let
          carry (l /\ a2b) = case l of
            h : t → ((t /\ (Just h)) /\ a2b)
            Nil → ((Nil /\ Nothing) /\ a2b)

          app ((t /\ a) /\ a2b) = t /\ a2b a
        in
          rmap app (rmap carry pf >>> Strong.first pa)

instance applicativeTokenized ∷ (Category p, Profunctor p, Strong p) ⇒ Applicative (Tokenized p i) where
  pure i =
    Tokenized
      $ let
          set (l /\ h) = case h of
            Just h' → ((h' : l) /\ i)
            Nothing → (l /\ i)
        in
          rmap set identity

instance altTokenized ∷ (Alt (p (Maybe i)), Category p, Costrong p, Strong p) ⇒ Alt (Tokenized p i) where
  alt (Tokenized p1) (Tokenized p2) = Tokenized $ unfirst (rmap (map $ const Nothing) p1) *** (unsecond p1 <|> unsecond p2)

uncons :: forall t6. List t6 -> List t6 /\ Maybe t6
uncons (h : t) = t /\ Just h

uncons Nil = Nil /\ Nothing

liftUntokenized :: forall i o p. Strong p => p (Maybe i) o -> Tokenized p i o
liftUntokenized p = Tokenized (Strong.second p)

unliftUntokenized :: forall i o p. Profunctor p => Tokenized p i o -> p (List i) o
unliftUntokenized (Tokenized v) = dimap uncons snd v
