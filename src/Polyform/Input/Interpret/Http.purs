module Polyform.Input.Interpret.Http where

import Prelude

import Data.Array (catMaybes, singleton)
import Data.Functor.Variant (VariantF)
import Data.Maybe (Maybe, fromMaybe)
import Data.NonEmpty (NonEmpty)
import Data.StrMap (StrMap, lookup)
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Polyform.Field.Validation.Combinators (int, required, scalar)
import Polyform.Input.Http (StringErr)
import Polyform.Input.Interpret.Validation (IntF(..), StringF(..), _int, _string)
import Polyform.Validation (V, Validation, hoistFn, runValidation)
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
  -> (V e v -> a)
  -> Validation m e Value v
  -> m a
_handleValue n query k validation =
  runValidation validation fieldQuery >>= (k >>> pure)
 where
  fieldQuery = fromMaybe [] (lookup (variantTag n) query)


handleString
  ∷ forall e m n
  . Monad m
  ⇒ StringF (Variant n) (Array (Variant (StringErr e))) Query
  ~> m
handleString (StringF n query k) =
  _handleValue n query k (hoistFn catMaybes >>> required singleton >>> scalar singleton)


type IntErr e = (scalar ∷ NonEmpty Array String, required ∷ Unit, int ∷ String | e)

handleInt
  ∷ forall e n m
  . Monad m
  ⇒ IntF (Variant n) (Array (Variant (IntErr e))) Query
  ~> m
handleInt (IntF n query k) =
  _handleValue n query k (hoistFn catMaybes >>> required singleton >>> scalar singleton >>> int singleton)

onField :: forall t139 t145 t146 t147 t154 t155 t156.
   Monad t155 => (VariantF t139 t147 -> t155 t147)
                 -> VariantF
                      ( string :: FProxy
                                    (StringF (Variant t154)
                                       (Array
                                          (Variant
                                             ( scalar :: NonEmpty Array String
                                             , required :: Unit
                                             | t156
                                             )
                                          )
                                       )
                                       (StrMap (Array (Maybe String)))
                                    )
                      , int :: FProxy
                                 (IntF (Variant t145)
                                    (Array
                                       (Variant
                                          ( scalar :: NonEmpty Array String
                                          , required :: Unit
                                          , int :: String
                                          | t146
                                          )
                                       )
                                    )
                                    (StrMap (Array (Maybe String)))
                                 )
                      | t139
                      )
                      t147
                    -> t155 t147
onField = on _int handleInt >>> on _string handleString

handle :: forall t103 t111 t112 t120 t121 t122.
   Monad t121 => VariantF
                   ( string :: FProxy
                                 (StringF (Variant t120)
                                    (Array
                                       (Variant
                                          ( scalar :: NonEmpty Array String
                                          , required :: Unit
                                          | t122
                                          )
                                       )
                                    )
                                    (StrMap (Array (Maybe String)))
                                 )
                   , int :: FProxy
                              (IntF (Variant t111)
                                 (Array
                                    (Variant
                                       ( scalar :: NonEmpty Array String
                                       , required :: Unit
                                       , int :: String
                                       | t112
                                       )
                                    )
                                 )
                                 (StrMap (Array (Maybe String)))
                              )
                   )
                   t103
                 -> t121 t103
handle =
  case_
    # on _int handleInt
    # on _string handleString
--     # on _optString  handleOptString
--     # on _optInt handleOptInt

interpret
  ∷ forall ei es n n' m
  . Monad m
  ⇒ Run
      ( string ∷ FProxy (StringF (Variant n) (Array (Variant (StringErr es))) Query)
      , int ∷ FProxy (IntF (Variant n') (Array (Variant (IntErr ei))) Query)
      )
  ~> m
interpret = Run.interpret handle
