import SubrowSpec (subrowSpec)
import SyntaxSpec (syntaxSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  subrowSpec
  syntaxSpec
