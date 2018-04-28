module Polyform.Validation where

import Prelude

import Control.Alt (class Alt)
import Control.Apply (lift2)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Either (Either(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)

data V e a = Invalid e | Valid e a
derive instance functorV ∷ Functor (V e)

instance bifunctorV ∷ Bifunctor V where
  bimap f _ (Invalid e) = Invalid (f e)
  bimap f g (Valid e a) = Valid (f e) (g a)

instance applyV ∷ (Semigroup e) ⇒ Apply (V e) where
  apply (Valid m1 f) (Valid m2 a) = Valid (m1 <> m2) (f a)
  apply (Invalid m1) (Valid m2 _) = Invalid (m1 <> m2)
  apply (Invalid m1) (Invalid m2) = Invalid (m1 <> m2)
  apply (Valid m1 _) (Invalid m2) = Invalid (m1 <> m2)

derive instance eqV ∷ (Eq e, Eq a) => Eq (V e a)
derive instance ordV ∷ (Ord e, Ord a) => Ord (V e a)

instance showV ∷ (Show e, Show a) => Show (V e a) where
  show (Invalid e) = "(Invalid " <> show e <> ")"
  show (Valid e a) = "(Valid " <> show e <> " " <> show a <> ")"

instance applicativeV ∷ (Monoid e) ⇒ Applicative (V e) where
  pure a = Valid mempty a

-- | Defaul `Semigroup` instance appends valid and invalid
-- | parts of our `V`.
instance semigroupV :: (Semigroup err, Semigroup a) => Semigroup (V err a) where
  append = lift2 append

instance monoidV :: (Monoid e, Monoid a) => Monoid (V e a) where
  mempty = pure mempty

isValid ∷ ∀ a e. V e a → Boolean
isValid (Valid _ _) = true
isValid _ = false

fromEither ∷ ∀ a e. (Monoid e) ⇒ Either e a → V e a
fromEither (Left e) = Invalid e
fromEither (Right a) = Valid mempty a

toEither ∷ ∀ a e. V e a → Either e a
toEither (Invalid e) = Left e
toEither (Valid _ a) = Right a

newtype Validation m e a b = Validation (a → m (V e b))
derive instance newtypeVaildation ∷ Newtype (Validation m e a b) _
derive instance functorValidation ∷ (Functor m) ⇒ Functor (Validation m e a)

instance applyValidation ∷ (Semigroup e, Monad m) ⇒ Apply (Validation m e a) where
  apply vf va = Validation $ \i → do
    vf' ← unwrap vf i
    va' ← unwrap va i
    pure $ vf' <*> va'

instance applicativeValidation ∷ (Monoid e, Monad m) ⇒ Applicative (Validation m e a) where
  pure = Validation <<< const <<< pure <<< pure

-- | Three potentatial instances of `Alt` for our Validation type.
-- |
-- | `AltAll` accumulates whole report but returns first valid result.
-- | It evaluates all expressions:
-- |
-- | pure (Valid e1 a1) <|> pure (Invalid e2) = Valid (e1 <> e2) a1
-- | pure (Valid e1 a1) <|> pure (Valid e2 a2) = Valid (e1 <> e2) a1
-- | pure (Invalid e1) <|> pure (Invalid e2) = Invalid (e1 <> e2)
-- |
-- | If we provide `Plus` instance for this type (where `empty = Invalid mempty`)
-- | this alt instance would break `Alternative` laws:
-- |
-- | ```purescript
-- | (Invalid [e] <*> pure unit) <|> (Invalid [e] <*> pure unit) = Invalid [e, e]
-- |
-- | but
-- |
-- | Invalid [e] <*> (pure unit <|> pure unit) = Invalid [e]
-- | ```

newtype AltAll m e a b = AltAll (Validation m e a b)
derive newtype instance functorAltAll ∷ (Functor m) ⇒ Functor (AltAll m e a)
derive instance newtypeAltAll ∷ Newtype (AltAll m e a b) _

instance altAltAll ∷ (Monoid e, Monad m) ⇒ Alt (AltAll m e a) where
  alt v1 v2 = AltAll $ Validation \a → do
    v1' ← unwrap (unwrap v1) a
    v2' ← unwrap (unwrap v2) a
    pure $ case v1', v2' of
      Valid m1 r, Valid m2 _ → Valid (m1 <> m2) r
      Valid m1 r, Invalid m2 → Valid (m1 <> m2) r
      Invalid m1, Valid m2 r → Valid (m1 <> m2) r
      Invalid m1, Invalid m2 → Invalid (m1 <> m2)

-- | `AltErrs` accumulates only errors report and drops it when encounters
-- | valid result:
-- |
-- | pure (Valid e1 a1) <|> _ = Valid e1 a1
-- | pure (Invalid e1) <|> Valid e2 a2 = Valid e2 a2
-- | pure (Invalid e1) <|> (Invalid e2) = Invalid (e1 <> e2)
-- |
-- | There is similar story with this instance as with `AltAll` - if we provide `Plus`
-- | it would break `Alternative` laws.
newtype AltErrs m e a b = AltErrs (Validation m e a b)
derive newtype instance functorAltErrs ∷ (Functor m) ⇒ Functor (AltErrs m e a)
derive instance newtypeAltErrs ∷ Newtype (AltErrs m e a b) _

instance altAltErrs ∷ (Monoid e, Monad m) ⇒ Alt (AltErrs m e a) where
  alt v1 v2 = AltErrs $ Validation \a → do
    v1' ← unwrap (unwrap v1) $ a
    case v1' of
      v@(Valid _ _) → pure v
      (Invalid i1) → do
        v2' ← unwrap (unwrap v2) a
        pure $ case v2' of
          v@(Valid _ _) → v
          (Invalid i2) → Invalid (i1 <> i2)

-- | `AltFirst` can't break `Alternative` laws instance as I think it
-- | doesn't have empty value. It returns first valid result or first invalid
-- | when all subvalidations fail:
-- |
-- | pure (Valid e1 a1) <|> _ = Valid e1 a1
-- | pure (Invalid e1) <|> Valid e2 a2 = Valid e2 a2
-- | pure (Invalid e1) <|> (Invalid e2) = Invalid e1
-- |
newtype AltFirst m e a b = AltFirst (Validation m e a b)
derive newtype instance functorAltFirst ∷ (Functor m) ⇒ Functor (AltFirst m e a)
derive instance newtypeAltFirst ∷ Newtype (AltFirst m e a b) _

instance altAltFirst ∷ (Monoid e, Monad m) ⇒ Alt (AltFirst m e a) where
  alt v1 v2 = AltFirst $ Validation \a → do
    v1' ← unwrap (unwrap v1) $ a
    case v1' of
      v@(Valid _ _) → pure v
      i@(Invalid m) → do
        v2' ← unwrap (unwrap v2) $ a
        pure $ case v2' of
          v@(Valid _ _) → v
          otherwise → i

instance semigroupValidation ∷ (Semigroup (m (V e b))) ⇒ Semigroup (Validation m e a b) where
  append (Validation v1) (Validation v2) = Validation (\a → v1 a <> v2 a)

instance monoidValidation ∷ (Applicative m, Monoid e, Monoid b, Semigroup (m (V e b))) ⇒ Monoid (Validation m e a b) where
  mempty = Validation <<< const <<< pure $ mempty

instance semigroupoidValidation ∷ (Monad m, Semigroup e) ⇒ Semigroupoid (Validation m e) where
  compose v2 v1 =
    Validation $ (\a → do
      eb ← unwrap v1 a
      case eb of
        Valid e b → do
          r ← unwrap v2 b
          pure $ case r of
            Valid e' c → Valid (e <> e') c
            Invalid e' → Invalid (e <> e')
        Invalid e → pure (Invalid e))

instance categoryValidation ∷ (Monad m, Monoid e) ⇒ Category (Validation m e) where
  id = Validation $ pure <<< pure

instance profunctorValidation ∷ (Monad m, Monoid e) ⇒ Profunctor (Validation m e) where
  dimap l r v = (hoistFn l) >>> v >>> (hoistFn r)

-- XXX: Provide Strong instance too
instance choiceValidation ∷ (Monad m, Monoid e) ⇒ Choice (Validation m e) where
  left v = Validation (case _ of
    Left i → map Left <$> runValidation v i
    Right r → pure (Valid mempty (Right r)))

  right v = Validation (case _ of
    Right i → map Right <$> runValidation v i
    Left l → pure (Valid mempty (Left l)))

runValidation ∷ ∀ a b e m. Validation m e a b → (a → m (V e b))
runValidation = unwrap

ask ∷ ∀ a e m. Monad m ⇒ Monoid e ⇒ Validation m e a a
ask = Validation (\a → pure (Valid mempty a))

hoistFn ∷ ∀ a e b m. Monad m ⇒ Monoid e ⇒ (a → b) → Validation m e a b
hoistFn f = Validation $ f >>> pure >>> pure

hoistFnV ∷ ∀ a e b m. Monad m ⇒ Monoid e ⇒ (a → V e b) → Validation m e a b
hoistFnV f = Validation $ f >>> pure

hoistFnMV ∷ ∀ a e b m. Monad m ⇒ Monoid e ⇒ (a → m (V e b)) → Validation m e a b
hoistFnMV f = Validation f

-- | Provides access to validation result
-- | so you can `bimap` over `e` and `b` type in resulting `V e b`.
newtype BifunctorValidation m a e b = BifunctorValidation (Validation m e a b)
derive instance newtypeBifunctorValidation ∷ Newtype (BifunctorValidation m a e b) _

instance bifunctorBifunctorValidation ∷ (Monad m) ⇒ Bifunctor (BifunctorValidation m a) where
  bimap l r (BifunctorValidation (Validation f)) = BifunctorValidation $ Validation $ \a → do
    v ← f a
    pure $ bimap l r v

bimapValidation ∷ ∀ a b b' e e' m
  . (Monad m)
  ⇒ (e → e')
  → (b → b')
  → Validation m e a b
  → Validation m e' a b'
bimapValidation l r = unwrap <<< bimap l r <<< BifunctorValidation


lmapValidation :: forall t167 t168 t169 t173 t174. Monad t174 => (t169 -> t168) -> Validation t174 t169 t173 t167 -> Validation t174 t168 t173 t167
lmapValidation l = unwrap <<< lmap l <<< BifunctorValidation

rmapValidation :: forall t100 t101 t94 t95 t96. Monad t101 => (t95 -> t94) -> Validation t101 t96 t100 t95 -> Validation t101 t96 t100 t94
rmapValidation l = unwrap <<< rmap l <<< BifunctorValidation
