module Polyform.Input.Interpret.Record where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(VariantRep), unsafeGet)
import Polyform.Input.Interpret.Validation (IntF(..), StringF(..), _int, _string)
import Run (FProxy, Run, VariantF, case_, on)
import Run as Run
import Type.Row (class RowToList, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)

class VariantFieldsType (rl ∷ RowList) (vo ∷ # Type) a | rl a → vo

instance a_variantFieldsTypeSame ∷ (VariantFieldsType rl vo' a, RowCons sym Unit vo' vo) ⇒ VariantFieldsType (Cons sym a rl) vo a
instance b_variantFieldsTypeDiff ∷ (VariantFieldsType rl vo a) ⇒ VariantFieldsType (Cons sym b rl) vo a
instance c_variantFieldsTypeNil ∷ VariantFieldsType Nil () a

onMatch
  ∷ ∀ a rl r v
  . RowToList r rl
  ⇒ VariantFieldsType rl v a
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
  ⇒ VariantFieldsType ql n Int
  ⇒ IntF (Variant n) (Variant e) (Record q) ~> m
handleInt (IntF n query k) =
  pure $ k value
 where
  value = Right $ onMatch query n


handleString
  ∷ forall e n m q ql
  . Monad m
  ⇒ RowToList q ql
  ⇒ VariantFieldsType ql n String
  ⇒ StringF (Variant n) (Variant e) (Record q) ~> m
handleString (StringF n query k) =
  pure $ k value
 where
  value = Right $ onMatch query n


handle
  ∷ forall ei es n n' m q ql
  . Monad m
  ⇒ RowToList q ql
  ⇒ VariantFieldsType ql n String
  ⇒ VariantFieldsType ql n' Int
  ⇒ VariantF
      ( string ∷ FProxy (StringF (Variant n) (Variant es) (Record q))
      , int ∷ FProxy (IntF (Variant n') (Variant ei) (Record q))
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
  ⇒ VariantFieldsType ql n String
  ⇒ VariantFieldsType ql n' Int
  ⇒ Run
      ( string ∷ FProxy (StringF (Variant n) (Variant e) (Record q))
      , int ∷ FProxy (IntF (Variant n') (Variant e) (Record q))
      )
      a
  → m a
interpret = Run.interpret handle

