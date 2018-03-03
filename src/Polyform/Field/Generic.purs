module Polyform.Field.Generic where

import Prelude

import Control.Monad.Reader.Class (ask)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.List (List, singleton)
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Record (insert)
import Data.Tuple (Tuple(Tuple))
import Polyform.Field as Field
import Polyform.Field.Validation (hoistEither)
import Type.Prelude (class IsSymbol, class RowLacks, Proxy(..), SProxy(..), reflectSymbol)

-- | This type class provides basic way to transform simple sum type
-- | (constructors without args are only allowed) into: `Validation` and
-- | `choices` array which can be used in `Choice` record.
-- |
class SingleChoice choice where
  choiceImpl ∷ choice → String
  choicesImpl ∷ Proxy choice → List (Tuple String choice)

instance singleChoiceSum ∷ (SingleChoice a, SingleChoice b) ⇒ SingleChoice (Sum a b) where
  choiceImpl (Inl v) = choiceImpl v
  choiceImpl (Inr v) = choiceImpl v

  choicesImpl _
    = map (Inl <$> _) (choicesImpl (Proxy ∷ Proxy a))
    <> map (Inr <$> _) (choicesImpl (Proxy ∷ Proxy b))

instance singleChoiceConstructor ∷ (IsSymbol name) ⇒ SingleChoice (Constructor name NoArguments) where
  choiceImpl _ = reflectSymbol (SProxy ∷ SProxy name)

  choicesImpl p =
    singleton (Tuple v c)
   where
    c = ((Constructor NoArguments) ∷ Constructor name NoArguments)
    v = reflectSymbol (SProxy ∷ SProxy name)

choice
  ∷ ∀ choice choiceRep
  . (Generic choice choiceRep)
  ⇒ (SingleChoice choiceRep)
  ⇒ choice
  → String
choice v = choiceImpl (from v)

choices
  ∷ ∀ choice choiceRep
  . Generic choice choiceRep
  ⇒ SingleChoice choiceRep
  ⇒ Proxy choice
  → List (Tuple String choice)
choices _ = map (to <$> _) (choicesImpl (Proxy ∷ Proxy choiceRep))

choiceParser
  ∷ ∀ a choiceRep m
  . Monad m
  ⇒ Generic a choiceRep
  ⇒ SingleChoice choiceRep
  ⇒ Proxy a
  → Field.Validation m String String a
choiceParser p =
  hoistEither \s → case lookup s (fromFoldable $ choices p) of
    Just o → pure o
    Nothing → Left s

class MultiChoice c (c' ∷ # Type) | c → c' where
  multiChoiceParserImpl
    ∷ ∀ m
    . Monad m
    ⇒ Proxy c
    → Field.Validation m String (Array String) { product ∷ (Record c') , checkChoice ∷ c → Boolean }

instance multiChoiceConstructor
  ∷ (IsSymbol name, RowCons name Boolean () row, RowLacks name ())
  ⇒ MultiChoice (Constructor name NoArguments) row where

  multiChoiceParserImpl proxy =
    parser
   where
    parser =
      let
        _name = (SProxy ∷ SProxy name)
        validate = any (reflectSymbol _name == _)
        product i = insert _name  (validate i) {}
        checkChoice i _ = validate i
      in hoistEither $ \i → pure { product: product i, checkChoice: checkChoice i }

instance multiChoiceSum
  ∷ (IsSymbol name, MultiChoice tail tailRow, RowCons name Boolean tailRow row, RowLacks name tailRow)
  ⇒ MultiChoice (Sum (Constructor name NoArguments) tail) row where

  multiChoiceParserImpl proxy =
    parser <$> ask <*> multiChoiceParserImpl (Proxy ∷ Proxy tail)
   where
    parser i { product, checkChoice } =
      let
        _name = (SProxy ∷ SProxy name)
        r = any (reflectSymbol _name == _) i
        checkChoice' = case _ of
          (Inl _) → r
          (Inr b) → checkChoice b
      in
        { product: insert _name r (product ∷ Record tailRow)
        , checkChoice: checkChoice'
        }

multiChoiceParser
  ∷ ∀ a choiceRep row m
  . (Monad m)
  ⇒ (Generic a choiceRep)
  ⇒ (MultiChoice choiceRep row)
  ⇒ Proxy a
  → Field.Validation m String (Array String) { product ∷ Record row, checkChoice ∷ a → Boolean }
multiChoiceParser _ =
  multiChoiceParserImpl (Proxy ∷ Proxy choiceRep) <#> \{ product, checkChoice } →
    { product, checkChoice: checkChoice <<< from }

