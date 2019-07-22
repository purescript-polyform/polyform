module Test.Main where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Array (all)
import Data.Either (Either)
import Data.Enum (class Enum, enumFromTo)
import Data.Identity (Identity)
import Data.Validation.Semigroup (V)
import Effect (Effect)
import Polyform.Validator (Validator(..))
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B, checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy2(..), Proxy3(..))
import Unsafe.Coerce (unsafeCoerce)


newtype AValidator e i o = AValidator (Validator Identity e i o)
instance eqAValidator ∷ (Eq o, Eq e, Bounded i, Enum i) ⇒ Eq (AValidator e i o) where
  eq (AValidator (Validator v1)) (AValidator (Validator v2)) =
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
      (AValidator <<< Validator <<< map (map fromEither)) <$> gf

derive newtype instance functorAValidator ∷ Functor (AValidator e i)
derive newtype instance applyAValidator ∷ (Monoid e) ⇒ Apply (AValidator e i)
derive newtype instance applicativeAValidator ∷ (Monoid e) ⇒ Applicative (AValidator e i)
derive newtype instance altAValidator ∷ (Monoid e) ⇒ Alt (AValidator e i)
derive newtype instance plusAValidator ∷ (Monoid e) ⇒ Plus (AValidator e i)
derive newtype instance semigroupoidAValidator ∷ (Monoid e) ⇒ Semigroupoid (AValidator e)

-- | check is going to fail on this
-- instance alternativeValidator ∷ (Monad m, Monoid e) ⇒ Alternative (Validator m e i)
-- derive newtype instance alternativeAValidator ∷ (Monoid e) ⇒ Alternative (AValidator e i)

checkArray ∷ Effect Unit
checkArray = checkLaws "Validator" do
  -- Data.checkOrd prxArray
  Data.checkFunctor prx2Validator
  Control.checkApply prx2Validator
  Control.checkAlt prx2Validator
  Control.checkSemigroupoid prx3Validator
  -- Control.checkAlternative prx2Validator
  Control.checkApplicative prx2Validator
  -- Control.checkBind prx2Array
  -- Control.checkMonad prx2Array
  -- Data.checkSemigroup prxArray
  -- Data.checkMonoid prxArray
  -- Control.checkPlus prx2Array
  -- Control.checkMonadZero prx2Array
  -- Control.checkMonadPlus prx2Array
  where
  -- prxValidator = Proxy ∷ Proxy (AValidator A B C)
  prx2Validator = Proxy2 ∷ Proxy2 (AValidator A B)
  prx3Validator = Proxy3 ∷ Proxy3 (AValidator A)

main :: Effect Unit
main = do
  checkArray
  -- runTest $ do
  -- suite "Test.Polyform.Validator.Dual" Dual.suite
