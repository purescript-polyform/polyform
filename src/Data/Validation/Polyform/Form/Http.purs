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
import Data.Validation.Polyform.Form (IntF(..), OptIntF(..), OptStringF(..), StringF(..), _int, _optInt, _optString, _string)
import Data.Validation.Polyform.Form as F
import Data.Validation.Polyform.Validation.Field (_required, int', missing, missing', opt, pureV, required', runFieldValidation, tag, validate)
import Data.Validation.Polyform.Validation.Field as FieldValidation
import Data.Variant (Variant, case_, inj, on)
import Type.Prelude (class IsSymbol, SProxy(..))

-- http query representation
type FieldQuery = Array (Maybe String)
type Query = StrMap FieldQuery

type Form m form a = F.Form m form Query a
type FieldValidation m e a = FieldValidation.FieldValidation m e FieldQuery a

_scalar = (SProxy ∷ SProxy "scalar")

scalar ∷ ∀ a m. (Monad m) ⇒ FieldValidation.FieldValidation m (Array a) (Array a) a
scalar = validate $ case _ of
  [a] → Right a
  arr → Left arr

scalar' ∷ ∀ a e m. (Monad m) ⇒ FieldValidation.FieldValidation m (Variant (scalar ∷ Array a | e)) (Array a) a
scalar' = tag _scalar scalar

handleString
  ∷ forall e n m
  . IsSymbol n
  ⇒ Monad m
  ⇒ StringF n Query (Variant ( scalar ∷ Array String | e)) ~> m
handleString (StringF n query k) =
  (runExceptT $ runFieldValidation (pureV catMaybes >>> scalar') fieldQuery) >>= (k >>> pure)
 where
  fieldQuery = fromMaybe [] (lookup (reflectSymbol n) query)

handleOptString
  ∷ ∀ e n m
  . IsSymbol n
  ⇒ Monad m
  ⇒ Applicative m
  ⇒ OptStringF n Query (Variant ( scalar ∷ Array (Maybe String) | e)) ~> m
handleOptString (OptStringF n query k) =
  (runExceptT $ runFieldValidation scalar' fieldQuery) >>= (k >>> pure)
 where
  fieldQuery = fromMaybe [Nothing] (lookup (reflectSymbol n) query)

handleInt
  ∷ forall e n m
  . IsSymbol n
  ⇒ Monad m
  ⇒ IntF n Query (Variant ( scalar ∷ Array String, int ∷ String | e)) ~> m
handleInt (IntF n query k) =
  runExceptT (runFieldValidation (pureV catMaybes >>> scalar' >>> int') fieldQuery) >>= (k >>> pure)
 where
  fieldQuery = fromMaybe [] (lookup (reflectSymbol n) query)

handleOptInt
  ∷ ∀ e n m
  . IsSymbol n
  ⇒ Monad m
  ⇒ Applicative m
  ⇒ OptIntF n Query (Variant ( scalar ∷ Array (Maybe String), int ∷ String | e)) ~> m
handleOptInt (OptIntF n query k) =
  (runExceptT $ runFieldValidation (scalar' >>> int) fieldQuery) >>= (k >>> pure)
 where
  fieldQuery = fromMaybe [Nothing] (lookup (reflectSymbol n) query)
  int = dimap (note Nothing) (either id Just) (right int')


handle =
  case_
    # on _string handleString
    # on _optString  handleOptString
    # on _int handleInt
    # on _optInt handleOptInt



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
