import SubrowSpec (subrowSpec)
import SyntaxSpec (syntaxSpec)
import Test.Hspec (hspec)

-- The QuickCheck specs

main :: IO ()
main = hspec $ do
  subrowSpec
  syntaxSpec
