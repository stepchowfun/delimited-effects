import ErrorSpec (errorSpec)
import InferenceSpec (inferenceSpec)
import SubrowSpec (subrowSpec)
import SubtypeSpec (subtypeSpec)
import SyntaxSpec (syntaxSpec)
import Test.Hspec (hspec)

-- The QuickCheck specs

main :: IO ()
main = hspec $ do
  errorSpec
  inferenceSpec
  subrowSpec
  subtypeSpec
  syntaxSpec
