import LexerSpec (lexerSpec)
import ParserSpec (parserSpec)
import SyntaxSpec (syntaxSpec)
import FSyntaxSpec (fSyntaxSpec)
import Test.Hspec (hspec)

-- The QuickCheck specs

main :: IO ()
main = hspec $ do
  fSyntaxSpec
  lexerSpec
  parserSpec
  syntaxSpec
