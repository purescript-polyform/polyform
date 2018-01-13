module Data.Validation.Polyform.Form.Http where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Validation.Polyform.Form (IntF(..), StringF(..), _int, _string)
import Data.Validation.Polyform.Validation.Field (runFieldValidation)
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..), unsafeGet, unsafeHas)
import Prelude (Unit)
import Run (FProxy(..), VariantF, case_, on, Run)
import Run as Run
import Type.Row (class RowToList, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)

class VariantMatchCases (rl ∷ RowList) (vo ∷ # Type) a | rl a → vo

instance a_variantMatchConsSame ∷ (VariantMatchCases rl vo' a, RowCons sym Unit vo' vo) ⇒ VariantMatchCases (Cons sym a rl) vo a
instance b_variantMatchConsDiff ∷ (VariantMatchCases rl vo a) ⇒ VariantMatchCases (Cons sym b rl) vo a
instance c_variantMatchNil ∷ VariantMatchCases Nil () a

onMatch
  ∷ ∀ a rl r v
  . RowToList r rl
  ⇒ VariantMatchCases rl v a
  ⇒ Record r
  → Variant v
  → a
onMatch r v =
  case coerceV v of
    VariantRep v' → unsafeGet v'.type r
 where
  coerceV ∷ ∀ b. Variant v → VariantRep b
  coerceV = unsafeCoerce

handleInt
  ∷ forall e n m q ql
  . Monad m
  ⇒ RowToList q ql
  ⇒ VariantMatchCases ql n Int
  ⇒ IntF (Variant n) (Variant e) (Record q) ~> m
handleInt (IntF n query k) =
  pure $ k value
 where
  value = Right $ onMatch query n


handleString
  ∷ forall e n m q ql
  . Monad m
  ⇒ RowToList q ql
  ⇒ VariantMatchCases ql n String
  ⇒ StringF (Variant n) (Variant e) (Record q) ~> m
handleString (StringF n query k) =
  pure $ k value
 where
  value = Right $ onMatch query n


handle
  ∷ forall e n n' m q ql
  . Monad m
  ⇒ RowToList q ql
  ⇒ VariantMatchCases ql n String
  ⇒ VariantMatchCases ql n' Int
  ⇒ VariantF
      ( string ∷ FProxy (StringF (Variant n) (Variant e) (Record q))
      , int ∷ FProxy (IntF (Variant n') (Variant e) (Record q))
      )
  ~> m
handle =
  case_
    # on _string handleString
    # on _int handleInt
    -- # on _optString  handleOptString
    -- # on _optInt handleOptInt


interpret
  ∷ forall a e n n' m q ql
  . Monad m
  ⇒ RowToList q ql
  ⇒ VariantMatchCases ql n String
  ⇒ VariantMatchCases ql n' Int
  ⇒ Run
      ( string ∷ FProxy (StringF (Variant n) (Variant e) (Record q))
      , int ∷ FProxy (IntF (Variant n') (Variant e) (Record q))
      )
      a
  → m a
interpret = Run.interpret handle
