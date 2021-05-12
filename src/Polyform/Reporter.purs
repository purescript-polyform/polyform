module Polyform.Reporter
  ( Reporter(..)
  , bimapReporter
  , hoist
  , liftFn
  , liftFnEither
  , liftFnMR
  , liftFnR
  , liftValidator
  , liftValidatorWith
  , liftValidatorWithM
  , lmapM
  , lmapReporter
  , runReporter
  , R
  , toValidator
  ) where

import Prelude
import Control.Alt (class Alt)
import Control.Lazy (class Lazy)
import Control.Monad.Maybe.Trans (MaybeT(..), mapMaybeT, runMaybeT)
import Control.Monad.Writer (WriterT(..), runWriterT)
import Control.Monad.Writer.Trans (mapWriterT)
import Control.Plus (class Plus)
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Either (Either(..))
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Star (Star(..), hoistStar)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..), validation)
import Polyform.Validator (Validator(..), runValidator)

newtype Reporter m r i o
  = Reporter (Star (MaybeT (WriterT r m)) i o)

derive instance newtypeReporter ∷ Newtype (Reporter m r i b) _
derive newtype instance functorReporter ∷ (Functor m) ⇒ Functor (Reporter m e i)

instance applyReporter ∷ (Monad m, Monoid e) ⇒ Apply (Reporter m e i) where
  -- | We need this instance as we want to execute
  -- | all validation steps.
  apply (Reporter (Star rf)) (Reporter (Star ra)) =
    Reporter
      $ Star \i →
          MaybeT
            $ do
                mf ← runMaybeT (rf i)
                ma ← runMaybeT (ra i)
                pure (mf <*> ma)

derive newtype instance applicativeReporter ∷ (Monad m, Monoid e) ⇒ Applicative (Reporter m e i)
derive newtype instance profunctorReporter ∷ Functor m ⇒ Profunctor (Reporter m e)
derive newtype instance choiceReporter ∷ (Monoid e, Monad m) ⇒ Choice (Reporter m e)
derive newtype instance strongReporter ∷ (Monad m, Semigroup e) ⇒ Strong (Reporter m e)
derive newtype instance altReporter ∷ (Monad m, Monoid r) ⇒ Alt (Reporter m r i)
-- where alt (Reporter v1) (Reporter v2) = Reporter $ v1 <|> v2
derive newtype instance plusReporter ∷ (Monad m, Monoid r) ⇒ Plus (Reporter m r i)

-- where empty = Reporter empty
type R r a
  = Tuple (Maybe a) r

liftV ∷ ∀ a r. Monoid r ⇒ V r a → R r a
liftV (V (Right a)) = Tuple (Just a) mempty

liftV (V (Left e)) = Tuple Nothing e

toV ∷ ∀ a r. R r a → V r a
toV (Tuple (Just a) _) = V (Right a)

toV (Tuple _ r) = V (Left r)

liftVWith ∷ ∀ a e r. (e → r) → (a → r) → V e a → R r a
liftVWith f g = validation (Tuple Nothing <<< f) (\a → Tuple (Just a) (g a))

runReporter ∷ ∀ i o m r. Reporter m r i o → (i → m (Tuple (Maybe o) r))
runReporter (Reporter (Star f)) = f >>> runMaybeT >>> runWriterT

instance semigroupReporter ∷ (Monad m, Monoid r, Monoid o) ⇒ Semigroup (Reporter m r i o) where
  append (Reporter (Star r1)) (Reporter (Star r2)) = Reporter (Star (\i → append <$> r1 i <*> r2 i))

instance monoidReporter ∷ (Monad m, Monoid r, Monoid o) ⇒ Monoid (Reporter m r i o) where
  mempty = Reporter $ Star $ const (pure mempty)

instance semigroupoidReporter ∷ (Monad m, Monoid r) ⇒ Semigroupoid (Reporter m r) where
  compose (Reporter v2) (Reporter v1) = Reporter (v2 <<< v1)

instance categoryReporter ∷ (Monad m, Monoid r) ⇒ Category (Reporter m r) where
  identity = Reporter identity

instance lazyReporter ∷ Lazy (Reporter m r i o) where
  defer f = Reporter (Star \i → let Reporter (Star r) = f unit in r i)

-- ask ∷ ∀ i m r. Applicative m ⇒ Monoid r ⇒ Reporter m r i i
-- ask = liftFn identity
hoist ∷ ∀ e i m m'. Functor m ⇒ (m ~> m') → Reporter m e i ~> Reporter m' e i
hoist nt (Reporter r) = Reporter (hoistStar (mapMaybeT (mapWriterT nt)) r)

-- | Building `Reporter` from `Validator` with possibly empty failure report.
liftValidator ∷ ∀ e i m. Functor m ⇒ Monoid e ⇒ Validator m e i ~> Reporter m e i
liftValidator (Validator (Star r)) = Reporter (Star (r >>> un Compose >>> map liftV >>> WriterT >>> MaybeT))

-- | Building `Reporter` from `Validator` by creating report from error and from value using also an input.
-- | We need only `Functor m` in this case.
liftValidatorWith ∷ ∀ e i m o r. Functor m ⇒ (Tuple i e → r) → (Tuple i o → r) → Validator m e i o → Reporter m r i o
liftValidatorWith f g (Validator (Star r)) =
  Reporter $ Star
    $ \i →
        (r >>> un Compose >>> map (liftVWith (f <<< Tuple i) (g <<< Tuple i)) >>> WriterT >>> MaybeT $ i)

-- | The same as above but in monadic context.
liftValidatorWithM ∷ ∀ e i m o r. Monad m ⇒ (Tuple i e → m r) → (Tuple i o → m r) → Validator m e i o → Reporter m r i o
liftValidatorWithM f g validator = Reporter $ Star run
  where
  run i =
    MaybeT $ WriterT $ runValidator validator i
      >>= case _ of
          V (Left e) → f (Tuple i e) >>= Tuple Nothing >>> pure
          V (Right o) → g (Tuple i o) >>= Tuple (Just o) >>> pure

toValidator ∷ ∀ e i m. Functor m ⇒ Monoid e ⇒ Reporter m e i ~> Validator m e i
toValidator (Reporter (Star f)) = Validator (Star (f >>> un MaybeT >>> un WriterT >>> map toV >>> Compose))

liftFn ∷ ∀ e i m. Monad m ⇒ Monoid e ⇒ (Function i) ~> Reporter m e i
liftFn f = Reporter $ Star $ f >>> pure

liftFnR ∷ ∀ e i m o. Monad m ⇒ Semigroup e ⇒ (i → R e o) → Reporter m e i o
liftFnR f = Reporter $ Star $ f >>> pure >>> WriterT >>> MaybeT

liftFnMR ∷ ∀ e i m o. (i → m (R e o)) → Reporter m e i o
liftFnMR f = Reporter $ Star $ f >>> WriterT >>> MaybeT

liftFnEither ∷ ∀ e i m o. Monad m ⇒ Monoid e ⇒ (i → Either e o) → Reporter m e i o
liftFnEither f = liftFnR $ f >>> liftEitherR
  where
  liftEitherR (Left e) = Tuple Nothing e

  liftEitherR (Right o) = Tuple (Just o) mempty

-- | Provides access to validation result so you can
-- | `bimap` over `r` and `b` type in resulting `R r b`.
newtype BifunctorReporter m i r o
  = BifunctorReporter (Reporter m r i o)

derive instance newtypeBifunctorReporter ∷ Newtype (BifunctorReporter m i r o) _

instance bifunctorBifunctorReporter ∷ Monad m ⇒ Bifunctor (BifunctorReporter m i) where
  bimap l r (BifunctorReporter rep) = BifunctorReporter $ Reporter $ Star run
    where
    run i =
      MaybeT
        $ WriterT do
            v ← runReporter rep i
            pure $ bimap (map r) l v

bimapReporter ∷
  ∀ i m o o' r r'.
  (Monad m) ⇒
  (r → r') →
  (o → o') →
  Reporter m r i o →
  Reporter m r' i o'
bimapReporter l r = unwrap <<< bimap l r <<< BifunctorReporter

lmapReporter ∷ ∀ i m o r r'. Monad m ⇒ (r → r') → Reporter m r i o → Reporter m r' i o
lmapReporter l = unwrap <<< lmap l <<< BifunctorReporter

lmapM ∷ ∀ i m r r'. Monad m ⇒ (r → m r') → Reporter m r i ~> Reporter m r' i
lmapM f reporter =
  liftFnMR $ runReporter reporter
    >=> case _ of
        Tuple (Just a) r → Tuple (Just a) <$> f r
        Tuple _ r → Tuple Nothing <$> f r
