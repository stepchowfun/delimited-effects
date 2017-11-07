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
                               -> Type Effect
                               -> Row Effect
                               -> Bool
specEffectMapLookupAfterExtend em z x t r =
  effectMapLookup (EMExtend em z x t r) z == Just (x, t, r)

specEffectMapExtendAfterLookup :: EffectMap Variable Effect
                               -> Effect
                               -> Effect
                               -> Bool
specEffectMapExtendAfterLookup em z1 z2 =
  case effectMapLookup em z1 of
    Just (x, t, r) ->
      effectMapLookup (EMExtend em z1 x t r) z2 == effectMapLookup em z2
    Nothing -> True

syntaxSpec :: Spec
syntaxSpec = modifyMaxSuccess (const 1000000) $ do
  describe "contextLookup" $ do
    it "contextLookup (CExtend c x t r) x == Just (t, r)" $
      property specContextLookupAfterExtend
    it "CExtend em z t r == CExtend (contextLookup em z) z t r" $
      property specContextExtendAfterLookup
  describe "effectMapLookup" $ do
    it "effectMapLookup (EMExtend em z x t r) x == Just (x, t, r)" $
      property specEffectMapLookupAfterExtend
    it "EMExtend em z x t r == EMExtend (effectMapLookup em z) z x t r" $
      property specEffectMapExtendAfterLookup
