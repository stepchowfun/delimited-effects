module InferenceSpec (inferenceSpec) where

import Lib
  ( Context(..)
  , EffectMap(..)
  , Row(..)
  , Term(..)
  , Type(..)
  , inferTypeAndRow
  , subrow
  , subtype )
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Types (Effect(..), Variable(..))

-- The QuickCheck specs

specTypeCheck :: Term Variable Effect
              -> Type Effect
              -> Row Effect
              -> Expectation
specTypeCheck e t1 r1 =
  let c = CEmpty :: Context Variable Effect
      em = EMEmpty :: EffectMap Variable Effect
  in case inferTypeAndRow c em e of
    Right (t2, r2) -> do
      subtype t2 t1 `shouldBe` True
      subrow r2 r1 `shouldBe` True
    _ -> True `shouldBe` False

inferenceSpec :: Spec
inferenceSpec = describe "infer" $ do
  it "gives the correct type and effect row for EUnit" $ do
    specTypeCheck EUnit TUnit REmpty
