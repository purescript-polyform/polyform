module Test.Polyform.Reporter where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Array (all)
import Data.Either (Either(..))
import Data.Enum (class Enum, enumFromTo)
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity)
import Data.Profunctor.Star (Star(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Polyform.Reporter (R(..), Reporter(..))
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B, C, checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Test.QuickCheck.Laws.Data as Data
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy2(..), Proxy3(..))
import Unsafe.Coerce (unsafeCoerce)

newtype AReporter e i o = AReporter (Reporter Identity e i o)
instance eqAReporter ∷ (Eq o, Eq e, Bounded i, Enum i) ⇒ Eq (AReporter e i o) where
  eq (AReporter (Reporter (Star v1))) (AReporter (Reporter (Star v2))) =
    let
      arr = (enumFromTo bottom top ∷ Array i)
    in
      all (\i → v1 i `eq` v2 i) $ arr

fromEither ∷ ∀ a e. Either e a → R e a
fromEither = unsafeCoerce

newtype Ar e i o = Ar (i → Identity (R e o))
instance arbitraryA ∷ (Arbitrary e, Coarbitrary i, Arbitrary o) ⇒ Arbitrary (AReporter e i o) where
  arbitrary =
    -- | Type annotation just for readability
    let
      gf = arbitrary ∷ Gen (i → Identity (Either e (Tuple e o)))
      toR (Left e) = Failure e
      toR (Right (Tuple e o)) = Success e o
    in
      AReporter <<< Reporter <<< Star <<< map (Compose <<< (map toR)) <$> gf

derive newtype instance functorAReporter ∷ (Semigroup e) ⇒ Functor (AReporter e i)
derive newtype instance applyAReporter ∷ (Monoid e) ⇒ Apply (AReporter e i)
derive newtype instance applicativeAReporter ∷ (Monoid e) ⇒ Applicative (AReporter e i)
derive newtype instance altAReporter ∷ (Monoid e) ⇒ Alt (AReporter e i)
derive newtype instance plusAReporter ∷ (Monoid e) ⇒ Plus (AReporter e i)
derive newtype instance semigroupoidAReporter ∷ (Monoid e) ⇒ Semigroupoid (AReporter e)
derive newtype instance semigroupReporter ∷ (Semigroup o, Monoid e) ⇒ Semigroup (AReporter e i o)
derive newtype instance monoidReporter ∷ (Monoid o, Monoid e) ⇒ Monoid (AReporter e i o)

suite ∷ Effect Unit
suite = checkLaws "Reporter" do
  -- Data.checkOrd prxArray
  Data.checkFunctor prx2Reporter
  Control.checkApply prx2Reporter
  Control.checkAlt prx2Reporter
  Control.checkSemigroupoid prx3Reporter
  -- | This failes
  -- Control.checkAlternative prx2Reporter
  Control.checkApplicative prx2Reporter
  Control.checkPlus prx2Reporter
  -- Control.checkBind prx2Reporter
  -- Control.checkMonad prx2Array
  Data.checkSemigroup prxReporter
  Data.checkMonoid prxReporter
  -- Control.checkMonadZero prx2Array
  -- Control.checkMonadPlus prx2Array
  where
  prxReporter = Proxy ∷ Proxy (AReporter A B C)
  prx2Reporter = Proxy2 ∷ Proxy2 (AReporter A B)
  prx3Reporter = Proxy3 ∷ Proxy3 (AReporter A)

