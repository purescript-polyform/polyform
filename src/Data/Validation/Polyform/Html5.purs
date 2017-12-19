module Data.Validation.Polyform.Html5 where

import Prelude

import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe, maybe)
import Data.Validation.Polyform.Http (HttpFormValidation, inputForm)
import Data.Validation.Polyform.Validation.Field (FieldValidation, int', pureV, scalar', tag, validate)
import Data.Variant (Variant)
import Type.Prelude (SProxy(..))

-- | Range can be used to represent `type="range"`  `type="number"`
-- | for Integer values
type RangeErr e = Variant (range ∷ Int | e)
data RangeType = Range | Number

type Range err attrs =
  { name ∷ String
  , min ∷ Maybe Int
  , max ∷ Maybe Int
  , step ∷ Int
  , type ∷ RangeType
  , value ∷ Either (RangeErr err) Int
  | attrs
  }

range
  ∷ ∀ attrs e m
  . (Monad m)
  ⇒ { min ∷ Maybe Int, max ∷ Maybe Int | attrs }
  → FieldValidation m Int Int Int
range r =
  validate inRange
 where
  check = (&&) <$> maybe (const true) (>) r.min <*> maybe (const true) (<) r.max
  inRange i
    | check i = Right i
    | otherwise = Left i

range'
  ∷ ∀ attrs e m
  . (Monad m)
  ⇒ { min ∷ Maybe Int, max ∷ Maybe Int | attrs }
  → FieldValidation m (RangeErr e) Int Int
range' = tag (SProxy ∷ SProxy "range") <<< range

rangeForm
  ∷ ∀ attrs err m
  . (Monad m)
  ⇒ Range (int ∷ String, scalar ∷ Array String | err) attrs
  → HttpFormValidation m (List (Range (int ∷ String, scalar ∷ Array String | err) attrs)) Int
rangeForm field =
  let
    validation = range' field <<< int' <<< scalar' <<< pureV catMaybes
  in
    inputForm field validation
