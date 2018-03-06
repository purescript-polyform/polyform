module Polyform.Field.Generic where

import Prelude

import Data.Array (filter, null)
import Data.Foldable (any)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.List (List, singleton)
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Record (insert)
import Data.Tuple (Tuple(Tuple))
import Polyform.Validation (V(Invalid, Valid), Validation, hoistFnV)
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
  → Validation m String String a
choiceParser p =
  hoistFnV \s → case lookup s (fromFoldable $ choices p) of
    Just o → Valid s o
    Nothing → Invalid s

class MultiChoice c (c' ∷ # Type) | c → c' where
  -- | This function doesn't report
  -- | invalid values (hence Unit as error type)
  -- | It only marks valid ones currently...
  multiChoiceParserImpl
    ∷ Proxy c
    → Array String
    → { result ∷
        { product ∷ (Record c')
        , checkChoice ∷ c → Boolean
        }
      , remaining ∷ Array String
      }

instance multiChoiceConstructor
  ∷ (IsSymbol name, RowCons name Boolean () row, RowLacks name ())
  ⇒ MultiChoice (Constructor name NoArguments) row where

  multiChoiceParserImpl proxy i =
    let
      _name = (SProxy ∷ SProxy name)
      name = reflectSymbol _name
      selected = any (name == _) i
      product = insert _name selected {}
    in
      { result:
          { product
          , checkChoice: const $ selected
          }
      , remaining: filter (name /= _) i
      }

instance multiChoiceSum
  ∷ (IsSymbol name, MultiChoice tail tailRow, RowCons name Boolean tailRow row, RowLacks name tailRow)
  ⇒ MultiChoice (Sum (Constructor name NoArguments) tail) row where

  multiChoiceParserImpl proxy =
    parser
   where
    parser i =
      let
        { result: { product, checkChoice }, remaining } = multiChoiceParserImpl (Proxy ∷ Proxy tail) i
        _name = SProxy ∷ SProxy name
        name = reflectSymbol _name
        r = any (name == _) remaining
        checkChoice' = case _ of
          (Inl _) → r
          (Inr b) → checkChoice b
      in
        { result:
            { product: insert _name r (product ∷ Record tailRow)
            , checkChoice: checkChoice'
            }
        , remaining: filter (name /= _) remaining
        }

multiChoiceParser
  ∷ ∀ a choiceRep row m
  . (Monad m)
  ⇒ (Generic a choiceRep)
  ⇒ (MultiChoice choiceRep row)
  ⇒ Proxy a
  → Validation m (Array String) (Array String) { product ∷ Record row, checkChoice ∷ a → Boolean }
multiChoiceParser _ =
  hoistFnV $ \i →
    let
      { result: { product, checkChoice }, remaining } =
        multiChoiceParserImpl (Proxy ∷ Proxy choiceRep) i
    in
      if null remaining
        then pure { product, checkChoice: checkChoice <<< from }
        else Invalid remaining

