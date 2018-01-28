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
specContextExtendAfterLookup c x1 x2 = case contextLookupType c x1 of
  Just (t, r) ->
    contextLookupType (CTExtend c x1 t r) x2 == contextLookupType c x2
  Nothing -> True

specEffectMapLookupAfterExtend :: EffectMap -> String -> Type -> Row -> Bool
specEffectMapLookupAfterExtend em a t r =
  effectMapLookup (EMExtend em a t r) a == Just (t, r)

specEffectMapExtendAfterLookup :: EffectMap -> String -> String -> Bool
specEffectMapExtendAfterLookup em a1 a2 = case effectMapLookup em a1 of
  Just (t, r) ->
    effectMapLookup (EMExtend em a1 t r) a2 == effectMapLookup em a2
  Nothing -> True

syntaxSpec :: Spec
syntaxSpec = modifyMaxSuccess (const 100000) $ do
  describe "contextLookupType" $ do
    it "contextLookupType (CTExtend c x t r) x == Just (t, r)" $ do
      property specContextLookupAfterExtend
    it "CTExtend em x t r == CTExtend (contextLookupType em x) x t r" $ do
      property specContextExtendAfterLookup
  describe "effectMapLookup" $ do
    it "effectMapLookup (EMExtend em a t r) a == Just (t, r)" $ do
      property specEffectMapLookupAfterExtend
    it "EMExtend em a t r == em where effectMapLookup em a = Just (t, r)" $ do
      property specEffectMapExtendAfterLookup
