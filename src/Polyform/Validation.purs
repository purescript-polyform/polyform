module Polyform.Validation where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Apply (lift2)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Either (Either(..))
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

-- | This instance uses first valid value and accumulates only in case of invalid
-- | results.
-- |
-- | pure (Valid e1 a1) <|> pure (Invalid e2) = Valid (e1 <> e2) a1
-- | pure (Valid e1 a1) <|> pure (Valid e2 a2) = Valid (e1 <> e2) a1
-- | pure (Invalid e1) <|> pure (Invalid e2) = Invalid (e1 <> e2)
-- |
-- | If you need "dual" strategy just use apply which "prefers" invalid results:
-- |
-- | pure (Valid e1 a1) *> pure (Invalid e2) = Invalid (e1 <> e2)
-- | pure (Invalid e1) *> pure (Valid e2 a2) = Invalid (e1 <> e2)
-- | pure (Valid e1 a1) *> pure (Valid e2 a2) = Valid (e1 <> e2) a2
-- |
-- | If you find any not accumulative instance useful please
-- | add a newtype wrapper and provide a PR with related tests.
-- |
-- | And maybe there is also a place for
-- | "`Alt` somewhat dual to our `Category`"
-- | which short circuits on the first valid result...
instance altV ∷ (Semigroup e) ⇒ Alt (V e) where
  alt (Valid m1 a) (Valid m2 _) = Valid (m1 <> m2) a
  alt (Valid m1 a) (Invalid m2) = Valid (m1 <> m2) a
  alt (Invalid m1) (Valid m2 a) = Valid (m1 <> m2) a
  alt (Invalid m1) (Invalid m2) = Invalid (m1 <> m2)

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

instance altValidation ∷ (Monoid e, Monad m) ⇒ Alt (Validation m e a) where
  alt v1 v2 = Validation \a → do
    v1' ← unwrap v1 a
    v2' ← unwrap v2 a
    pure $ v1' <|> v2'

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
  identity = Validation $ pure <<< pure

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
