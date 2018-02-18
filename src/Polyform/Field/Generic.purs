module Polyform.Field.Generic where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader.Class (ask)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.List (List(..), singleton, (:))
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Record (insert)
import Data.Tuple (Tuple(Tuple))
import Polyform.Field as Field
import Polyform.Validation (V(..), liftV)
import Type.Prelude (class IsSymbol, class RowLacks, Proxy(..), SProxy(..), reflectSymbol)

-- | This type class provides basic way to transform simple sum type
-- | (constructors without args are only allowed) into: `Validation` and
-- | `options` array which can be used in `Choice` record.
-- |
class Option opt where
  optionImpl ∷ opt → String
  optionsImpl ∷ Proxy opt → List (Tuple String opt)

instance asOptionSum ∷ (Option a, Option b) ⇒ Option (Sum a b) where
  optionImpl (Inl v) = optionImpl v
  optionImpl (Inr v) = optionImpl v

  optionsImpl _
    = map (Inl <$> _) (optionsImpl (Proxy ∷ Proxy a))
    <> map (Inr <$> _) (optionsImpl (Proxy ∷ Proxy b))

instance asOptionConstructor ∷ (IsSymbol name) ⇒ Option (Constructor name NoArguments) where
  optionImpl _ = reflectSymbol (SProxy ∷ SProxy name)

  optionsImpl p =
    singleton (Tuple value option)
   where
    option = ((Constructor NoArguments) ∷ Constructor name NoArguments)
    value = reflectSymbol (SProxy ∷ SProxy name)


option ∷ ∀ opt optRep. (Generic opt optRep) ⇒ (Option optRep) ⇒ opt → String
option v = optionImpl (from v)

options ∷ ∀ a aRep. (Generic a aRep) ⇒ (Option aRep) ⇒ Proxy a → List (Tuple String a)
options _ = map (to <$> _) (optionsImpl (Proxy ∷ Proxy aRep))

optionsParser
  ∷ ∀ a aRep m
  . (Monad m)
  ⇒ (Generic a aRep)
  ⇒ (Option aRep)
  ⇒ Proxy a
  → Field.Validation m String String a
optionsParser p =
  liftV \s → case lookup s (fromFoldable $ options p) of
    Just o → pure o
    Nothing → Invalid (s : Nil)

-- 
-- class Choices c (c' ∷ # Type) | c → c' where
--   choicesParserImpl ∷ ∀ m. (Monad m) ⇒ (Proxy c) → FieldValidation m String (Array String) { product ∷ (Record c'), checkOpt ∷ c → Boolean }
-- 
-- instance choicesConstructor ∷ (IsSymbol name, RowCons name Boolean () row, RowLacks name ()) ⇒ Choices (Constructor name NoArguments) row where
--   choicesParserImpl proxy =
--     parser
--    where
--     parser =
--       let
--         _name = (SProxy ∷ SProxy name)
--         v = any (reflectSymbol _name == _)
--         product i = insert _name  (v i) {}
--         checkOpt i _ = v i
--       in pureV $ \i → { product: product i, checkOpt: checkOpt i }
-- 
-- instance choicesSum ∷ (IsSymbol name, Choices b br, RowCons name Boolean br row, RowLacks name br) ⇒ Choices (Sum (Constructor name NoArguments) b) row where
--   choicesParserImpl proxy =
--     parser
--    where
--     parser = do
--       i ← ask
--       { product, checkOpt } ← choicesParserImpl (Proxy ∷ Proxy b)
--       let
--         _name = (SProxy ∷ SProxy name)
--         v = any (reflectSymbol _name == _)
--         checkOpt' = case _ of
--           (Inl _) → v i
--           (Inr b) → checkOpt b
--       pure $ { product: insert _name  (v i) product, checkOpt: checkOpt' }
-- 
-- choicesParser ∷ ∀ a aRep row m
--   . (Monad m)
--   ⇒ (Generic a aRep)
--   ⇒ (Choices aRep row)
--   ⇒ Proxy a
--   → FieldValidation m String (Array String) { product ∷ Record row, checkOpt ∷ a → Boolean }
-- choicesParser _ = do
--   { product, checkOpt } ← choicesParserImpl (Proxy ∷ Proxy aRep)
--   pure { product, checkOpt: checkOpt <<< from }
-- 
