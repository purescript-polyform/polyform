module Polyform.Field.Generic.Option where

import Prelude

import Control.Alternative (class Alternative, empty)
import Control.Monad.Reader.Class (ask)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.List (List, singleton, (:))
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (second)
import Data.Record (insert)
import Data.StrMap (lookup, fromFoldable)
import Data.Tuple (Tuple(Tuple))
import Partial.Unsafe (unsafeCrashWith)
import Polyform.Field.Generic (class MultiChoice, class SingleChoice, choiceImpl, choicesImpl, multiChoiceParserImpl)
import Polyform.Field.Validation (Validation, liftEither, liftPure)
import Type.Prelude (class IsSymbol, class RowLacks, Proxy(..), SProxy(..), reflectSymbol)
import Unsafe.Coerce (unsafeCoerce)

-- | "Custom sum" type (heavly inspired by (or ripped from) purescript-variant internals)
-- | which allows you to easily provide options (together with order)
-- | for you Choice/Multichoice fields "without" defining dedicated type.
data Nil
data Cons (s ∷ Symbol) tail

infixr 8 type Cons as :-

class DropOpt (s ∷ Symbol) opts opts' | s opts → opts'
instance _a_headOptCons ∷ (IsSymbol s) ⇒ DropOpt s (s :- tail) tail
instance _b_tailOptCons ∷ (IsSymbol s, DropOpt s tail tail') ⇒ DropOpt s (head :- tail) (head :- tail')

class Elem (n ∷ Symbol) l
instance _a_headOnList ∷ (IsSymbol n) ⇒ Elem n (n :- tail)
instance _b_elemOnList ∷ (Elem n tail, IsSymbol n) ⇒ Elem n (head :- tail)

option ∷ ∀ opt opts. (Elem opt opts) ⇒ (IsSymbol opt) ⇒ Proxy opts → SProxy opt → Option opts
option opts opt = unsafeCoerce (reflectSymbol opt)

-- | `Option` is our symbol list carrier
foreign import data Option ∷ Type → Type

pop
  ∷ ∀ f opts opts' sym
  . Alternative f
  ⇒ IsSymbol sym
  ⇒ DropOpt sym opts opts'
  ⇒ SProxy sym
  → Option opts
  → f (Option opts')
pop p o = case prj p o of
  Nothing → empty
  Just _ → pure (unsafeCoerce o)

prepend
  ∷ ∀ opts sym
  . SProxy sym
  → Option opts
  → Option (sym :- opts)
prepend = unsafeCoerce

case_ ∷ ∀ a. Option Nil → a
case_ r = unsafeCrashWith case unsafeCoerce r of
  l → "Polyform.Field.Opt: pattern match failure [" <> l <> "]"

on
  ∷ ∀ a opts opts' sym
  . IsSymbol sym
  ⇒ DropOpt sym opts opts'
  ⇒ SProxy sym
  → a
  → (Option opts' → a)
  → Option opts
  → a
on p a f opt
  | reflectSymbol p == unsafeCoerce opt = a
  | otherwise = f (unsafeCoerce opt)

prj
  ∷ ∀ a opts opts' f sym
  . IsSymbol sym
  ⇒ DropOpt sym opts opts'
  ⇒ Alternative f
  ⇒ SProxy sym
  → Option opts
  → f a
prj p = on p empty (const empty)

instance eqOptionNil ∷ Eq (Option Nil) where
  eq a b = false

instance eqOptionCons
  ∷ (IsSymbol head, Eq (Option tail))
  ⇒ Eq (Option (head :- tail)) where

  eq a b =
    let _name = (SProxy ∷ SProxy head)
    in case pop _name a, pop _name b of
      Nothing, Nothing → true
      Just a', Just b' → eq a' b'
      _, _ → false

instance _a_optionsEnd
  ∷ (IsSymbol name, SingleChoice tail)
  ⇒ SingleChoice (Option (name :- Nil)) where

  choiceImpl _ = reflectSymbol (SProxy ∷ SProxy name)

  choicesImpl p =
    singleton (Tuple value opt)
   where
    value = reflectSymbol (SProxy ∷ SProxy name)
    opt = option (Proxy ∷ Proxy (name :- Nil)) (SProxy ∷ SProxy name)

instance _b_optionsRecurse
  ∷ (IsSymbol name, SingleChoice (Option tail))
  ⇒ SingleChoice (Option (name :- tail)) where

  choiceImpl _ = reflectSymbol (SProxy ∷ SProxy name)

  choicesImpl p =
    -- | We should use just unsafeCoerce here as this is what prepend really does
    (Tuple value opt) : map (second (prepend _name)) (choicesImpl (Proxy ∷ Proxy (Option tail)))
   where
    _name = SProxy ∷ SProxy name
    value = reflectSymbol (SProxy ∷ SProxy name)
    opt = option (Proxy ∷ Proxy (name :- tail)) (SProxy ∷ SProxy name)

choice
  ∷ ∀ opt
  . SingleChoice (Option opt)
  ⇒ Option opt
  → String
choice = choiceImpl

choices
  ∷ ∀ opt
  . SingleChoice (Option opt)
  ⇒ Proxy opt
  → List (Tuple String (Option opt))
choices _ =
  choicesImpl (Proxy ∷ Proxy (Option opt))

choicesParser
  ∷ ∀ m opt
  . Monad m
  ⇒ SingleChoice (Option opt)
  ⇒ Proxy opt
  → Validation m String String (Option opt)
choicesParser p =
  liftEither \s → case lookup s (fromFoldable $ choices p) of
    Just o → pure o
    Nothing → Left s

instance _a_choicesNil
  ∷ (IsSymbol name, RowCons name Boolean () row, RowLacks name ())
  ⇒ MultiChoice (Option (name :- Nil)) row where

  multiChoiceParserImpl proxy =
    parser
   where
    parser =
      let
        _name = (SProxy ∷ SProxy name)
        v = any (reflectSymbol _name == _)
        product i = insert _name  (v i) {}
        checkChoice i = case_ # on _name (v i)
      in liftPure $ \i → { product: product i, checkChoice: checkChoice i}

instance _b_choicesRecurse
  ∷ (IsSymbol name, MultiChoice (Option tail) br, RowCons name Boolean br row, RowLacks name br)
  ⇒ MultiChoice (Option (name :- tail)) row where

  multiChoiceParserImpl proxy =
    parser
   where
    parser = do
      { product, checkChoice } ← multiChoiceParserImpl (Proxy ∷ Proxy (Option tail))
      i ← ask
      let
        _name = (SProxy ∷ SProxy name)
        v = any (reflectSymbol _name == _)
        product' = insert _name  (v i) product
        checkChoice' = on _name (v i) checkChoice
      pure $ { product: product', checkChoice: checkChoice' }

multiChoicesParser
  ∷ ∀ m opt row. (Monad m)
  ⇒ MultiChoice (Option opt) row
  ⇒ Proxy opt
  → Validation m String (Array String) { checkChoice ∷ Option opt → Boolean, product ∷ Record row }
multiChoicesParser _ = multiChoiceParserImpl (Proxy ∷ Proxy (Option opt))

