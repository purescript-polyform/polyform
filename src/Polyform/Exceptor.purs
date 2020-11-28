module Polyform.Exceptor where

import Prelude
import Control.Alt (class Alt)
import Control.Lazy (class Lazy)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Either (Either(..))
import Data.Functor.Compose (Compose(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (class Strong)

-- | This type is nearly `Star (Except e m)` but:
-- |
-- | * No `Semigroup e` constraint in its `Alt` instance (consistent with `Either`)
-- |
-- | * Provides `Parallel` instance.
newtype Exceptor m e i o
  = Exceptor (Star (ExceptT e m) i o)

derive instance newtypeExceptor ∷ Newtype (Exceptor m r i o) _
derive newtype instance functorExceptor ∷ Applicative m ⇒ Functor (Exceptor m e i)
derive newtype instance applyExceptor ∷ Monad m ⇒ Apply (Exceptor m e i)
derive newtype instance applicativeExceptor ∷ Monad m ⇒ Applicative (Exceptor m e i)
derive newtype instance bindExceptor ∷ Monad m ⇒ Bind (Exceptor m e i)
derive newtype instance monadExceptor ∷ Monad m ⇒ Monad (Exceptor m e i)
derive newtype instance semigroupoidExceptor ∷ Monad m ⇒ Semigroupoid (Exceptor m e)
derive newtype instance categoryExceptor ∷ Monad m ⇒ Category (Exceptor m e)
derive newtype instance profunctorExceptor ∷ Functor m ⇒ Profunctor (Exceptor m e)
derive newtype instance choiceExceptor ∷ Monad m ⇒ Choice (Exceptor m e)
derive newtype instance strongExceptor ∷ Monad m ⇒ Strong (Exceptor m e)

instance lazyExceptor ∷ Monad m ⇒ Lazy (Exceptor m e i o) where
  defer f = Exceptor (Star \i → let Exceptor (Star f') = f unit in f' i)

instance altExceptor ∷ (Monad m) ⇒ Alt (Exceptor m e i) where
  alt (Exceptor (Star v1)) (Exceptor (Star v2)) =
    Exceptor
      $ Star \i →
          ExceptT do
            runExceptT (v1 i)
              >>= case _ of
                  (Right o1) → pure $ pure o1
                  (Left e1) → runExceptT (v2 i)

newtype ParExceptor f e i o
  = ParExceptor (Star (Compose f (Either e)) i o)

derive newtype instance functorParExceptor ∷ Applicative f ⇒ Functor (ParExceptor f e i)
derive newtype instance applyParExceptor ∷ Applicative f ⇒ Apply (ParExceptor f e i)
derive newtype instance applicativeParExceptor ∷ Applicative f ⇒ Applicative (ParExceptor f e i)

instance parallelExceptor ∷ (Applicative f, Parallel f m, Monad m) ⇒ Parallel (ParExceptor f e i) (Exceptor m e i) where
  parallel (Exceptor (Star f)) = ParExceptor $ Star (Compose <<< parallel <<< unwrap <$> f)
  sequential (ParExceptor (Star f)) = Exceptor $ Star $ (ExceptT <<< sequential <<< unwrap <$> f)
