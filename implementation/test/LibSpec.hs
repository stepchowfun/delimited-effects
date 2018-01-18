import ErrorSpec (errorSpec)
import InferenceSpec (inferenceSpec)
import LexerSpec (lexerSpec)
import ParserSpec (parserSpec)
import SubrowSpec (subrowSpec)
import SyntaxSpec (syntaxSpec)
import Test.Hspec (hspec)

-- The QuickCheck specs
main :: IO ()
main =
  hspec $ do
    errorSpec
    inferenceSpec
    lexerSpec
    parserSpec
    subrowSpec
    syntaxSpec
