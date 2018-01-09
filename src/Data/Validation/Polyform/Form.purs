module Data.Validation.Polyform.Form where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List(..), singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Record (insert, set)
import Data.Tuple (Tuple(..))
import Data.Validation.Polyform.Field (class Choices, class Options, ChoiceField, MultiChoiceField, choicesParser, options, optionsParser)
import Data.Validation.Polyform.Field.Option (Option)
import Data.Validation.Polyform.Field.Option as SymbolOption
import Data.Validation.Polyform.Validation.Field (FieldValidation, runFieldValidation, tag)
import Data.Validation.Polyform.Validation.Form (V(..), Validation(..), bimapResult)
import Data.Validation.Polyform.Validation.Form as FV
import Data.Variant (Variant, inj)
import Type.Prelude (class IsSymbol, Proxy(..), SProxy(SProxy))

-- | This module provides some helpers for building basic HTML forms.
-- |
-- | All builders operate on really trivial form type
-- | `type Form field = List field`, because we want to
-- | ease the transition between `FieldValidation` and `Validation`.

newtype Form m e q v =
  Form
    { validation ∷ Validation m e q v
    , default ∷ e
    }

derive instance newtypeForm ∷ Newtype (Form m e a b) _
derive instance functorForm ∷ (Functor m) ⇒ Functor (Form m e a)


instance applyForm ∷ (Semigroup e, Monad m) ⇒ Apply (Form m e a) where
  apply (Form rf) (Form ra) =
    Form
      { validation: rf.validation <*> ra.validation
      , default: rf.default <> ra.default
      }

instance applicativeForm ∷ (Monoid e, Monad m) ⇒ Applicative (Form m e a) where
  pure a = Form { validation: pure a, default: mempty }

instance altForm ∷ (Semigroup e, Alt m) ⇒ Alt (Form m e a) where
  alt (Form r1) (Form r2) = Form
    { validation: r1.validation <|> r2.validation
    , default: r1.default <> r2.default
    }

instance semigroupoidForm ∷ (Monad m, Semigroup e) ⇒ Semigroupoid (Form m e) where
  compose (Form r2) (Form r1) =
    Form { default: r1.default <> r2.default, validation: compose r2.validation r1.validation }

instance categoryForm ∷ (Monad m, Monoid e) ⇒ Category (Form m e) where
  id = Form { validation: id, default: mempty }

fromValidation :: forall a b e m. Monoid e => Validation m e a b -> Form m e a b
fromValidation validation = Form { validation, default: mempty }

bimap ∷ ∀ e e' i i' m o o'. (Monad m) ⇒ (e → e') → (o → o') → Form m e i o → Form m e' i o'
bimap f g (Form r) = Form
  { validation: bimapResult f g r.validation
  , default: f r.default
  }

inputForm
  ∷ ∀ attrs e form m q v
  . Monad m
  ⇒ (Record (value ∷ Either e v | attrs) → form)
  → Record (value ∷ Either e v | attrs)
  → FieldValidation m e q v
  → Form m form q v
inputForm singleton field validation = Form $
  { validation: Validation $ \query → do
      r ← runExceptT (runFieldValidation validation query)
      pure $ case r of
        Left e → Invalid (singleton $ field { value = Left e })
        Right v → Valid (singleton $ field { value = Right v }) v
  , default: singleton field
  }

-- newtype Component m e q q' v = Component ((q → q') → Validation m e q' v)

-- inputForm'
--   ∷ ∀ attrs e n m o o' q v
--   . Monad m
--   ⇒ RowCons n (Record (value ∷ Either e v | attrs)) o o'
--   ⇒ IsSymbol n
--   ⇒ SProxy n
--   → Record (value ∷ Either e v | attrs)
--   → FieldValidation m e q v
--   → Validation m (Form (Variant o')) (Maybe q) (Maybe v)
-- inputForm' p r =
--   bimapResult (inj p <$> _) id <<< inputForm r
-- 
-- _invalidOption = SProxy ∷ SProxy "invalidOption"
-- 
-- -- coproductChoiceForm ∷ ∀ a attrs e q rep m
-- --   . Monad m
-- --   ⇒ Generic a rep
-- --   ⇒ ChoiceField e opt attrs
-- --   → FieldValidation m (Variant (invalidOption ∷ String | e)) q String
-- --   → Validation m (List (ChoiceField (Variant (invalidOption ∷ String | e)) a attrs)) (Maybe q) (Maybe a)
-- -- coproductChoiceForm field validation = Validation
-- --   let
-- --     _p = Proxy ∷ Proxy a
-- --     opts = fromMaybe (options _p) field.options
-- --     field' = set (SProxy ∷ SProxy "options") opts field
-- --   in
-- --     case _ of
-- --       Nothing → pure $ Valid (singleton field') Nothing
-- --       Just query → do
-- --         let
-- --           optsValidation = tag _invalidOption $ optionsParser _p
-- --           validation' = optsValidation <<< validation
-- --         r ← runExceptT (runFieldValidation validation' query)
-- --         pure $ case r of
-- --           Left e → Invalid (singleton (field' { value = Left e }))
-- --           Right v → Valid (singleton (field' { value = Right v })) (Just v)
-- -- 
-- -- symbolChoiceForm ∷ ∀ a e i m
-- --   . Monad m
-- --   ⇒ Options (Option a)
-- --   ⇒ ChoiceField (Variant (invalidOption ∷ String | e)) (Option a)
-- --   → Proxy a
-- --   → FieldValidation m (Variant (invalidOption ∷ String | e)) i String
-- --   → Validation m (Form (ChoiceField (Variant (invalidOption ∷ String | e)) (Option a))) (Maybe i) (Maybe (Option a))
-- -- symbolChoiceForm field validation = Validation
-- --   let
-- --     p = Proxy ∷ Proxy a
-- --     field' =
-- --       case field.options of
-- --         Nil → field { options = options p }
-- --         otherwise → field
-- --   in
-- --     case _ of
-- --       Nothing → pure $ Valid (singleton field') Nothing
-- --       Just query → do
-- --         let
-- --           optsValidation = tag _invalidOption $ SymbolOption.optionsParser p
-- --           validation' = optsValidation <<< validation
-- --         r ← runExceptT (runFieldValidation validation' query)
-- --         pure $ case r of
-- --           Left e → Invalid (singleton (field' { value = Left e }))
-- --           Right v → Valid (singleton (field' { value = Right v })) (Just v)
-- 
-- -- coproductMultiChoiceForm ∷ ∀ a e i rep row m
-- --   . Monad m
-- --   ⇒ Generic a rep
-- --   ⇒ Choices rep row
-- --   ⇒ Options rep
-- --   ⇒ String
-- --   → Proxy a
-- --   → FieldValidation m (Variant (invalidOption ∷ String | e)) i (Array String)
-- --   → Validation m (Form (MultiChoice (Variant (invalidOption ∷ String | e)) a)) i (Record row)
-- -- coproductMultiChoiceForm name p validation = Validation $ \query → do
-- --   let
-- --     validation' = validation >>> (tag _invalidOption $ choicesParser p)
-- --     opts = options p
-- --   r ← runExceptT (runFieldValidation validation' query)
-- --   pure $ case r of
-- --     Left e → Invalid (singleton {name, choices: opts, value: Left e})
-- --     Right { product, checkOpt } → Valid (singleton {name, choices: opts, value: Right checkOpt}) product
-- -- 
-- -- symbolMultiChoiceForm ∷ ∀ a e i row m
-- --   . Monad m
-- --   ⇒ Choices (Option a) row
-- --   ⇒ Options (Option a)
-- --   ⇒ String
-- --   → Proxy a
-- --   → FieldValidation m (Variant (invalidOption ∷ String | e)) i (Array String)
-- --   → Validation m (Form (MultiChoice (Variant (invalidOption ∷ String | e)) (Option a))) i (Record row)
-- -- symbolMultiChoiceForm name p validation = Validation $ \query → do
-- --   let
-- --     validation' = validation >>> (tag _invalidOption $ SymbolOption.choicesParser p)
-- --     opts = SymbolOption.options p
-- --   r ← runExceptT (runFieldValidation validation' query)
-- --   pure $ case r of
-- --     Left e → Invalid (singleton { name, choices: opts, value: Left e })
-- --     Right { product, checkOpt } → Valid (singleton { name, choices: opts, value: Right checkOpt }) product
-- -- 
