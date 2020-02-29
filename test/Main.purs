module Test.Main where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Array (all)
import Data.Either (Either)
import Data.Enum (class Enum, enumFromTo)
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity)
import Data.Profunctor.Star (Star(..))
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Polyform.Validator (Validator(..))
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B, C, checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Test.QuickCheck.Laws.Data as Data
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy2(..), Proxy3(..))
import Unsafe.Coerce (unsafeCoerce)

newtype AValidator e i o = AValidator (Validator Identity e i o)
instance eqAValidator ∷ (Eq o, Eq e, Bounded i, Enum i) ⇒ Eq (AValidator e i o) where
  eq (AValidator (Validator (Star v1))) (AValidator (Validator (Star v2))) =
    let
      arr = (enumFromTo bottom top ∷ Array i)
    in
      all (\i → v1 i `eq` v2 i) $ arr

fromEither ∷ ∀ a e. Either e a → V e a
fromEither = unsafeCoerce

newtype Ar e i o = Ar (i → Identity (V e o))
instance arbitraryA ∷ (Arbitrary e, Coarbitrary i, Arbitrary o) ⇒ Arbitrary (AValidator e i o) where
  arbitrary =
    -- | Type annotation just for readability
    let
      gf = arbitrary ∷ Gen (i → Identity (Either e o))
    in
      AValidator <<< Validator <<< Star <<< map (Compose <<< (map V)) <$> gf

derive newtype instance functorAValidator ∷ (Semigroup e) ⇒ Functor (AValidator e i)
derive newtype instance applyAValidator ∷ (Monoid e) ⇒ Apply (AValidator e i)
derive newtype instance applicativeAValidator ∷ (Monoid e) ⇒ Applicative (AValidator e i)
derive newtype instance altAValidator ∷ (Monoid e) ⇒ Alt (AValidator e i)
derive newtype instance plusAValidator ∷ (Monoid e) ⇒ Plus (AValidator e i)
derive newtype instance semigroupoidAValidator ∷ (Monoid e) ⇒ Semigroupoid (AValidator e)
derive newtype instance semigroupValidator ∷ (Semigroup o, Monoid e) ⇒ Semigroup (AValidator e i o)
derive newtype instance monoidValidator ∷ (Monoid o, Monoid e) ⇒ Monoid (AValidator e i o)

-- | check is going to fail on this
-- instance alternativeAValidator ∷ (Monoid e) ⇒ Alternative (AValidator e i)

checkArray ∷ Effect Unit
checkArray = checkLaws "Validator" do
  -- Data.checkOrd prxArray
  Data.checkFunctor prx2Validator
  Control.checkApply prx2Validator
  Control.checkAlt prx2Validator
  Control.checkSemigroupoid prx3Validator
  -- | This failes
  -- Control.checkAlternative prx2Validator
  Control.checkApplicative prx2Validator
  Control.checkPlus prx2Validator
  -- Control.checkBind prx2Validator
  -- Control.checkMonad prx2Array
  Data.checkSemigroup prxValidator
  Data.checkMonoid prxValidator
  -- Control.checkMonadZero prx2Array
  -- Control.checkMonadPlus prx2Array
  where
  prxValidator = Proxy ∷ Proxy (AValidator A B C)
  prx2Validator = Proxy2 ∷ Proxy2 (AValidator A B)
  prx3Validator = Proxy3 ∷ Proxy3 (AValidator A)

main :: Effect Unit
main = do
  checkArray
  -- runTest $ do
  -- suite "Test.Polyform.Validator.Dual" Dual.suite
