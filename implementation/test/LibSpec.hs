import LexerSpec (lexerSpec)
import ParserSpec (parserSpec)
import SyntaxSpec (syntaxSpec)
import Test.Hspec (hspec)

-- The QuickCheck specs
main :: IO ()
main =
  hspec $ do
    lexerSpec
    parserSpec
    syntaxSpec
