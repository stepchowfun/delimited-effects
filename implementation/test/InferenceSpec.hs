module InferenceSpec (inferenceSpec) where

import Test.Hspec (Spec, describe, it)

-- The QuickCheck specs

inferenceSpec :: Spec
inferenceSpec = describe "infer" $ do
  it "gives the correct type and effect row" $ pending
