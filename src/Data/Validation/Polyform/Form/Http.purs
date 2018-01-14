module Data.Validation.Polyform.Form.Http where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Except (runExceptT)
import Control.Plus (empty)
import Data.Array (catMaybes, uncons)
import Data.Either (Either(..), either, note)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (right)
import Data.StrMap (StrMap, lookup)
import Data.Symbol (reflectSymbol)
import Data.Validation.Polyform.Form (IntF(..), StringF(..), _int, _string)
import Data.Validation.Polyform.Form as F
import Data.Validation.Polyform.Validation.Field (_required, int', missing, missing', opt, pureV, required', runFieldValidation, scalar', tag, validate)
import Data.Validation.Polyform.Validation.Field as FieldValidation
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Run (FProxy(..), VariantF, case_, on, Run)
import Run as Run
import Type.Prelude (class IsSymbol, SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

variantTag ∷ ∀ v. Variant v → String
variantTag v =
  let VariantRep r = coerceV v
  in r.type
 where
  coerceV ∷ Variant v → VariantRep Unit
  coerceV = unsafeCoerce

-- http query representation
type FieldQuery = Array (Maybe String)
type Query = StrMap FieldQuery

type Form m form a = F.Form m form Query a
type FieldValidation m e a = FieldValidation.FieldValidation m e FieldQuery a

-- _scalar = (SProxy ∷ SProxy "scalar")
-- 
-- scalar ∷ ∀ a m. (Monad m) ⇒ FieldValidation.FieldValidation m (Array a) (Array a) a
-- scalar = validate $ case _ of
--   [a] → Right a
--   arr → Left arr
-- 
-- scalar' ∷ ∀ a e m. (Monad m) ⇒ FieldValidation.FieldValidation m (Variant (scalar ∷ Array a | e)) (Array a) a
-- scalar' = tag _scalar scalar

type StringErr e = (scalar ∷ NonEmpty Array String, required ∷ Unit, int ∷ String | e)
handleString
  ∷ forall e n m
  . Monad m
 ⇒ StringF (Variant n) (Variant (StringErr e)) Query ~> m
handleString (StringF n query k) =
  (runExceptT $ runFieldValidation (pureV catMaybes >>> required' >>> scalar') fieldQuery) >>= (k >>> pure)
 where
  fieldQuery = fromMaybe [] (lookup (variantTag n) query)

-- handleOptString
--   ∷ ∀ e n m
--   . IsSymbol n
--   ⇒ Monad m
--   ⇒ Applicative m
--   ⇒ OptStringF n Query (Variant ( scalar ∷ Array (Maybe String) | e)) ~> m
-- handleOptString (OptStringF n query k) =
--   (runExceptT $ runFieldValidation scalar' fieldQuery) >>= (k >>> pure)
--  where
--   fieldQuery = fromMaybe [Nothing] (lookup (reflectSymbol n) query)

type IntErr e = (scalar ∷ NonEmpty Array String, required ∷ Unit, int ∷ String | e)

handleInt
  ∷ forall e n m
  . Monad m
  ⇒ IntF (Variant n) (Variant (IntErr e)) Query ~> m
handleInt (IntF n query k) =
  runExceptT (runFieldValidation (pureV catMaybes >>> required' >>> scalar' >>> int') fieldQuery) >>= (k >>> pure)
 where
  fieldQuery = fromMaybe [] (lookup (variantTag n) query)

-- handleOptInt
--   ∷ ∀ e n m
--   . IsSymbol n
--   ⇒ Monad m
--   ⇒ Applicative m
--   ⇒ OptIntF n Query (Variant ( scalar ∷ Array (Maybe String), int ∷ String | e)) ~> m
-- handleOptInt (OptIntF n query k) =
--   (runExceptT $ runFieldValidation (scalar' >>> int) fieldQuery) >>= (k >>> pure)
--  where
--   fieldQuery = fromMaybe [Nothing] (lookup (reflectSymbol n) query)
--   int = dimap (note Nothing) (either id Just) (right int')
-- 
handle =
  case_
    # on _int handleInt
    # on _string handleString
--     # on _optString  handleOptString
--     # on _optInt handleOptInt

interpret
  ∷ forall a e n n' m q ql
  . Monad m
  ⇒ Run
      ( string ∷ FProxy (StringF (Variant n) (Variant (StringErr e)) Query)
      , int ∷ FProxy (IntF (Variant n') (Variant (IntErr e)) Query)
      )
      a
  → m a
interpret = Run.interpret handle




-- rangeForm
--   ∷ ∀ attrs eff err form m name o q
--   . Monad m
--   ⇒ Monoid form
--   ⇒ IsSymbol name
--   ⇒ (Variant (rangeInput ∷ RangeInput (int ∷ String, scalar ∷ NonEmpty Array String, required ∷ Unit | err) attrs name | o) → form)
--   → RangeInput (int ∷ String, scalar ∷ NonEmpty Array String, required ∷ Unit | err) attrs name
--   → Form.Form (Run (string ∷ FProxy (StringF name q (Variant (int ∷ String, scalar ∷ NonEmpty Array String, required ∷ Unit | err))) | eff)) form q Int
-- rangeForm singleton field =
--   let
--     validation = rangeInput field <<< int'
--   in
--    Form.inputForm
--       (\field → singleton (inj _rangeInput field))
--       field
--       validation
-- 
