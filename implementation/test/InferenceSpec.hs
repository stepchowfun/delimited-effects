module InferenceSpec (inferenceSpec) where

import Lib
  ( Context(..)
  , EffectMap(..)
  , Row(..)
  , Term(..)
  , Type(..)
  , infer )
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Types (Effect(..), Variable(..))

-- The QuickCheck specs

specInfer :: Term Variable Effect
              -> Type Effect
              -> Expectation
specInfer e t1 =
  let c = CEmpty :: Context Variable Effect
      em = EMEmpty :: EffectMap Variable Effect
  in case infer c em e REmpty of
    Right (t2, _, _) -> do
      t1 == t2 `shouldBe` True
    _ -> True `shouldBe` False

inferenceSpec :: Spec
inferenceSpec = describe "infer" $ do
  it "gives the correct type and effect row for ETrue" $ do
    specInfer ETrue TBool
