module Polyform.Parsers.Validator where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Validation.Semigroup (V, invalid, unV)
import Polyform.Validator (valid)

-- | It seems that by giving more power to this type over input transformation
-- | than it is done in case of `Poliform.Validator` we loose some funny properties
-- | of applicative chains. We are not able to use `*>` as a conjunction of
-- | validators anymore.

-- | It is an experiment - we wrap whole result into `V` so we are able
-- | to provide `Semigroupid` instance
newtype Validator m e i o = Validator (i → m (V e { result ∷ o, i ∷ i }))
derive instance newtypeValidator ∷ Newtype (Validator m e i o) _
derive instance functorValidator ∷ (Functor m) ⇒ Functor (Validator m e i)

instance applyValidator ∷ (Semigroup e, Monad m) ⇒ Apply (Validator m e i) where
  apply vf va = Validator $ \i → do
    vf' ← unwrap vf i
    unV
      (pure <<< invalid)
      (\{ result: f, i: i'} → map (\r → r { result=f r.result }) <$> unwrap va i')
      vf'

instance applicativeValidator ∷ (Monoid e, Monad m) ⇒ Applicative (Validator m e i) where
  pure o = Validator \i → pure <<< valid $ { result:  o, i }

instance altValidator ∷ (Semigroup e, Monad m) ⇒ Alt (Validator m e i) where
  alt (Validator v1) (Validator v2) = Validator \i → do
    r ← v1 i
    unV (const $ v2 i) (pure <<< valid) r

instance plusValidator ∷ (Monad m, Monoid i, Monoid r) ⇒ Plus (Validator m r i) where
  empty = Validator <<< const <<< pure $ invalid mempty

instance semigroupValidator ∷ (Apply m, Semigroup e, Semigroup i, Semigroup o) ⇒ Semigroup (Validator m e i o) where
  append (Validator v1) (Validator v2) = Validator (\i → (<>) <$> v1 i <*> v2 i)

instance monoidValidator ∷ (Applicative m, Monoid e, Monoid i, Monoid o) ⇒ Monoid (Validator m e i o) where
  mempty = Validator <<< const <<< pure $ mempty

instance categoryValidator ∷ (Monad m, Monoid e) ⇒ Category (Validator m e) where
  identity = Validator \i → pure <<< valid $ { result: i, i }

instance semigroupoidValidator ∷ (Monad m, Monoid e) ⇒ Semigroupoid (Validator m e) where
  compose (Validator v2) (Validator v1) =
    Validator $ \i → do
      res ← v1 i
      unV
        (pure <<< invalid)
        (\o → do
          res' ← v2 o.result
          unV
            (pure <<< invalid)
            (\o' → pure $ valid { i: o.i, result: o'.result })
            res')
        res

instance profunctorValidator ∷ (Monad m, Monoid e) ⇒ Profunctor (Validator m e) where
  dimap l r v = (hoistFn' l) >>> v >>> (hoistFn' r)

-- | Two versions of hoisting:
-- | * first consumes whole input (requires Monoid)
-- | * second copies whole input
-- |
-- | I'm not sure if they both can be useful... we will see ;-)
hoistFn ∷ ∀ e i m o. Monoid i ⇒ Monad m ⇒ Monoid e ⇒ (i → o) → Validator m e i o
hoistFn f = Validator $ f >>> { result: _, i: mempty } >>> pure >>> pure

hoistFnV ∷ ∀ e i m o. Monoid i ⇒ Monad m ⇒ (i → V e o) → Validator m e i o
hoistFnV f = Validator $ f >>> map { result: _, i: mempty } >>> pure

hoistFnMV ∷ ∀ e i m o. Monad m ⇒ Monoid i ⇒ (i → m (V e o)) → Validator m e i o
hoistFnMV f = Validator $ f >>> map (map { result: _, i: mempty })

hoistFn' ∷ ∀ e i m o. Monad m ⇒ Monoid e ⇒ (i → o) → Validator m e i o
hoistFn' f = Validator $ (\i → { result: f i, i }) >>> pure >>> pure

hoistFnV' ∷ ∀ e i m o. Monad m ⇒ (i → V e o) → Validator m e i o
hoistFnV' f = Validator $ (\i → map { result: _, i: i } (f i)) >>> pure

hoistFnMV' ∷ ∀ e i m o. Monad m ⇒ (i → m (V e o)) → Validator m e i o
hoistFnMV' f = Validator $ \i → map (map { result: _, i }) (f i)
