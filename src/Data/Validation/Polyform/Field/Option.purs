module Data.Validation.Polyform.Field.Option where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (class Alternative, empty)
import Control.Monad.Reader.Class (ask)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.List (List, singleton, (:))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (second)
import Data.Record (insert)
import Data.Tuple (Tuple(Tuple))
import Data.Validation.Polyform.Field (class Choices, class Options, choicesParserImpl, optionsImpl, optionsParserImpl, toOptionValueImpl)
import Data.Validation.Polyform.Validation.Field (FieldValidation, Last(Last), pureV, validate, withException)
import Partial.Unsafe (unsafeCrashWith)
import Type.Prelude (class IsSymbol, class RowLacks, Proxy(..), SProxy(..), reflectSymbol)
import Unsafe.Coerce (unsafeCoerce)


-- | "Custom sum" type (some code shamlessly ripped from purescript-variant)
-- | which allows you to easily provide options (together with order)
-- | for you Choice/Multichoice fields.

data Nil
data Cons (s ∷ Symbol) tail

infixr 8 type Cons as :-

foreign import data Option ∷ Type → Type

class DropOpt (s ∷ Symbol) opts opts' | s opts → opts'
instance _a_headOptCons ∷ (IsSymbol s) ⇒ DropOpt s (s :- tail) tail
instance _b_tailOptCons ∷ (IsSymbol s, DropOpt s tail tail') ⇒ DropOpt s (head :- tail) (head :- tail')

class Elem (n ∷ Symbol) l
instance _a_headOnList ∷ (IsSymbol n) ⇒ Elem n (n :- tail)
instance _b_elemOnList ∷ (Elem n tail, IsSymbol n) ⇒ Elem n (head :- tail)

option ∷ ∀ opt opts. (Elem opt opts) ⇒ (IsSymbol opt) ⇒ Proxy opts → SProxy opt → Option opts
option opts opt = unsafeCoerce (reflectSymbol opt)

-- | You can always extend option type safely for given value
prepend
  ∷ ∀ opts sym
  . SProxy sym
  → Option opts
  → Option (sym :- opts)
prepend = unsafeCoerce

case_ ∷ ∀ a. Option Nil → a
case_ r = unsafeCrashWith case unsafeCoerce r of
  l → "Polyform.Opt: pattern match failure [" <> l <> "]"

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

instance _a_optionsEnd ∷ (IsSymbol name, Options tail) ⇒ Options (Option (name :- Nil)) where
  toOptionValueImpl _ = reflectSymbol (SProxy ∷ SProxy name)

  optionsImpl p =
    singleton (Tuple value opt)
   where
    value = reflectSymbol (SProxy ∷ SProxy name)
    opt = option (Proxy ∷ Proxy (name :- Nil)) (SProxy ∷ SProxy name)

  optionsParserImpl _ =
    validate \s →
      if s == value
        then
          Right opt
        else
          Left (Last s)
   where
    opt = option (Proxy ∷ Proxy (name :- Nil)) (SProxy ∷ SProxy name)
    value = reflectSymbol (SProxy ∷ SProxy name)

instance _b_optionsRecurse ∷ (IsSymbol name, Options (Option tail)) ⇒ Options (Option (name :- tail)) where

  toOptionValueImpl _ = reflectSymbol (SProxy ∷ SProxy name)

  optionsImpl p =
    -- | We should use just unsafeCoerce here as this is what prepend really does
    (Tuple value opt) : map (second (prepend _name)) (optionsImpl (Proxy ∷ Proxy (Option tail)))
   where
    _name = SProxy ∷ SProxy name
    value = reflectSymbol (SProxy ∷ SProxy name)
    opt = option (Proxy ∷ Proxy (name :- tail)) (SProxy ∷ SProxy name)

  optionsParserImpl _ =
    (validate \s →
      if s == value
        then
          Right opt
        else
          Left (Last s)) <|> (prepend _name <$> (optionsParserImpl (Proxy ∷ Proxy (Option tail))))
   where
    _name = SProxy ∷ SProxy name
    opt = option (Proxy ∷ Proxy (name :- tail)) (SProxy ∷ SProxy name)
    value = reflectSymbol (SProxy ∷ SProxy name)


options :: forall opt. Options opt => Proxy opt -> List (Tuple String opt)
options = optionsImpl

optionsParser :: forall m opt. Options opt => Monad m => Proxy opt -> FieldValidation m String String opt
optionsParser = withException unwrap <<< optionsParserImpl

toOptionValue :: forall opt. Options opt => opt -> String
toOptionValue = toOptionValueImpl

instance _a_choicesNil ∷ (IsSymbol name, RowCons name Boolean () row, RowLacks name ()) ⇒ Choices (Option (name :- Nil)) row where
  choicesParserImpl proxy =
    parser
   where
    parser =
      let
        _name = (SProxy ∷ SProxy name)
        v = any (reflectSymbol _name == _)
      in pureV $ \i → insert _name  (v i) {}

instance _b_choicesRecurse ∷ (IsSymbol name, Choices (Option tail) br, RowCons name Boolean br row, RowLacks name br) ⇒ Choices (Option (name :- tail)) row where
  choicesParserImpl proxy =
    parser
   where
    parser = do
      let
        _name = (SProxy ∷ SProxy name)
        v = any (reflectSymbol _name == _)
      i ← ask
      r ← choicesParserImpl (Proxy ∷ Proxy (Option tail))
      pure $ insert _name  (v i) r

choicesParser ∷ ∀ m opt row. (Monad m) ⇒ Choices (Option opt) row ⇒ Proxy opt → FieldValidation m String (Array String) (Record row)
choicesParser _ = choicesParserImpl (Proxy ∷ Proxy (Option opt))
