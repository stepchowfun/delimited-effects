module SyntaxSpec (syntaxSpec) where

import Lib
  ( Context(..)
  , EffectMap(..)
  , Row(..)
  , Type(..)
  , contextLookupType
  , effectMapLookup )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (property)

-- The QuickCheck specs

specContextLookupAfterExtend :: Context -> String -> Type -> Row -> Bool
specContextLookupAfterExtend c x t r =
  contextLookupType (CTExtend c x t r) x == Just (t, r)

specContextExtendAfterLookup :: Context -> String -> String -> Bool
specContextExtendAfterLookup c x1 x2 =
  case contextLookupType c x1 of
    Just (t, r) ->
      contextLookupType (CTExtend c x1 t r) x2 == contextLookupType c x2
    Nothing -> True

specEffectMapLookupAfterExtend :: EffectMap -> String -> String -> Bool
specEffectMapLookupAfterExtend em z x =
  effectMapLookup (EMExtend em z x) z == Just x

specEffectMapExtendAfterLookup :: EffectMap -> String -> String -> Bool
specEffectMapExtendAfterLookup em z1 z2 =
  case effectMapLookup em z1 of
    Just x ->
      effectMapLookup (EMExtend em z1 x) z2 == effectMapLookup em z2
    Nothing -> True

syntaxSpec :: Spec
syntaxSpec = modifyMaxSuccess (const 100000) $ do
  describe "contextLookupType" $ do
    it "contextLookupType (CTExtend c x t r) x == Just (t, r)" $ do
      property specContextLookupAfterExtend
    it "CTExtend em x t r == CTExtend (contextLookupType em x) x t r" $ do
      property specContextExtendAfterLookup
  describe "effectMapLookup" $ do
    it "effectMapLookup (EMExtend em z x) z == Just x" $ do
      property specEffectMapLookupAfterExtend
    it "EMExtend em z x == EMExtend (effectMapLookup em z) z x" $ do
      property specEffectMapExtendAfterLookup
