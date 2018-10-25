module Polyform.Parsers.Validator where

import Prelude

import Control.Alt (class Alt)
import Data.Newtype (class Newtype, unwrap)
import Data.Validation.Semigroup (V, isValid)
import Polyform.Validator (valid)

-- | It seems that by giving more power to this type for input transformation
-- | than it is done in case of `Poliform.Validator` we loose some funny properties
-- | of applicative chains. We are not able to use `*>` as a conjunction of
-- | validators anymore.

newtype Validator m e i o = Validator (i → m { result ∷ V e o, i ∷ i })
derive instance newtypeValidator ∷ Newtype (Validator m e i o) _
derive instance functorValidator ∷ (Functor m) ⇒ Functor (Validator m e i)

instance applyValidator ∷ (Semigroup e, Monad m) ⇒ Apply (Validator m e i) where
  apply vf va = Validator $ \i → do
    { result: rf, i: i' } ← unwrap vf i
    { result: ra, i: i'' } ← unwrap va i'
    pure
      { result:  rf <*> ra
      , i: i''
      }

instance applicativeValidator ∷ (Monoid e, Monad m) ⇒ Applicative (Validator m e i) where
  pure o = Validator \i → pure { result: valid o, i }

instance altValidator ∷ (Semigroup e, Monad m) ⇒ Alt (Validator m e i) where
  alt (Validator v1) (Validator v2) = Validator \i → do
    r@{ result: r1, i: i1 } ← v1 i
    if isValid r1
      then pure r
      else
        v2 i
