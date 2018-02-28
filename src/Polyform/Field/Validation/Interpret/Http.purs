module Polyform.Field.Validation.Interpret.Http where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Array (catMaybes)
import Data.Either (Either)
import Data.Maybe (Maybe, fromMaybe)
import Data.NonEmpty (NonEmpty)
import Data.StrMap (StrMap, lookup)
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Polyform.Field.Validation.Interpret (IntF(..), StringF(..), _int, _string)
import Polyform.Field.Validation (Validation, liftPure, required, runValidation, scalar)
import Polyform.Field.Validation.Combinators (int)
import Run (FProxy, Run, case_, on)
import Run as Run
import Unsafe.Coerce (unsafeCoerce)

-- | This representation should cover
-- | possible http query values:
-- | `?field`, `?field=`, `?field=value`,
-- | `?field=value1&field=value2`
type Value = Array (Maybe String)
type Query = StrMap Value

variantTag ∷ ∀ v. Variant v → String
variantTag v =
  let VariantRep r = coerceV v
  in r.type
 where
  coerceV ∷ Variant v → VariantRep Unit
  coerceV = unsafeCoerce

_handleValue :: forall a e n m v
  . Monad m
  => Variant n
  -> Query
  -> (Either e v -> a)
  -> Validation m e Value v
  -> m a
_handleValue n query k validation =
  (runExceptT $ runValidation validation fieldQuery) >>= (k >>> pure)
 where
  fieldQuery = fromMaybe [] (lookup (variantTag n) query)


type StringErr e = (scalar ∷ NonEmpty Array String, required ∷ Unit | e)

handleString
  ∷ forall e m n
  . Monad m
  ⇒ StringF (Variant n) (Variant (StringErr e)) Query ~> m
handleString (StringF n query k) =
  _handleValue n query k (liftPure catMaybes >>> required >>> scalar)


type IntErr e = (scalar ∷ NonEmpty Array String, required ∷ Unit, int ∷ String | e)

handleInt
  ∷ forall e n m
  . Monad m
  ⇒ IntF (Variant n) (Variant (IntErr e)) Query ~> m
handleInt (IntF n query k) =
  _handleValue n query k (liftPure catMaybes >>> required >>> scalar >>> int)

onField = on _int handleInt >>> on _string handleString

handle =
  case_
    # on _int handleInt
    # on _string handleString
--     # on _optString  handleOptString
--     # on _optInt handleOptInt

interpret
  ∷ forall a ei es n n' m
  . Monad m
  ⇒ Run
      ( string ∷ FProxy (StringF (Variant n) (Variant (StringErr es)) Query)
      , int ∷ FProxy (IntF (Variant n') (Variant (IntErr ei)) Query)
      )
      a
  → m a
interpret = Run.interpret handle
