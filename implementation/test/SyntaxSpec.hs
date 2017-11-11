module SyntaxSpec (syntaxSpec) where

import Lib
  ( Context(..)
  , EffectMap(..)
  , Row(..)
  , Type(..)
  , contextLookup
  , effectMapLookup )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (property)
import Types (Effect(..), Variable(..))

-- The QuickCheck specs

specContextLookupAfterExtend :: Context Variable Effect
                             -> Variable
                             -> Type Effect
                             -> Row Effect
                             -> Bool
specContextLookupAfterExtend c x t r =
  contextLookup (CExtend c x t r) x == Just (t, r)

specContextExtendAfterLookup :: Context Variable Effect
                             -> Variable
                             -> Variable
                             -> Bool
specContextExtendAfterLookup c x1 x2 =
  case contextLookup c x1 of
    Just (t, r) -> contextLookup (CExtend c x1 t r) x2 == contextLookup c x2
    Nothing -> True

specEffectMapLookupAfterExtend :: EffectMap Variable Effect
                               -> Effect
                               -> Variable
                               -> Bool
specEffectMapLookupAfterExtend em z x =
  effectMapLookup (EMExtend em z x) z == Just x

specEffectMapExtendAfterLookup :: EffectMap Variable Effect
                               -> Effect
                               -> Effect
                               -> Bool
specEffectMapExtendAfterLookup em z1 z2 =
  case effectMapLookup em z1 of
    Just x ->
      effectMapLookup (EMExtend em z1 x) z2 == effectMapLookup em z2
    Nothing -> True

syntaxSpec :: Spec
syntaxSpec = modifyMaxSuccess (const 100000) $ do
  describe "contextLookup" $ do
    it "contextLookup (CExtend c x t r) x == Just (t, r)" $ do
      property specContextLookupAfterExtend
    it "CExtend em x t r == CExtend (contextLookup em x) x t r" $ do
      property specContextExtendAfterLookup
  describe "effectMapLookup" $ do
    it "effectMapLookup (EMExtend em z x) z == Just x" $ do
      property specEffectMapLookupAfterExtend
    it "EMExtend em z x == EMExtend (effectMapLookup em z) z x" $ do
      property specEffectMapExtendAfterLookup
