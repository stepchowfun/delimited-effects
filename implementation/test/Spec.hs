import Lib (Row(..), rowEquiv)
import Test.Hspec (describe, hspec, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (Arbitrary, arbitrary, elements, property)

data Effect = EffectA | EffectB | EffectC
  deriving (Eq, Show)

data Variable = VariableA | VariableB | VariableC
  deriving (Eq, Show)

instance Arbitrary Effect where
  arbitrary = elements [EffectA, EffectB, EffectC]

instance Arbitrary Variable where
  arbitrary = elements [VariableA, VariableB, VariableC]

eqEquiv :: Row Effect Variable -> Row Effect Variable -> Bool
eqEquiv x y = x /= y || rowEquiv x y

main :: IO ()
main = hspec $ modifyMaxSuccess (const 1000000) $ do
  describe "rowEquiv" $ do
    it "returns True for equal rows" $ property eqEquiv
