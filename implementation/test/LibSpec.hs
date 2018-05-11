import EvaluationSpec (evaluationSpec)
import InferenceSpec (inferenceSpec)
import LexerSpec (lexerSpec)
import ParserSpec (parserSpec)
import SubstitutionSpec (substitutionSpec)
import SyntaxSpec (syntaxSpec)
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    evaluationSpec
    inferenceSpec
    lexerSpec
    parserSpec
    substitutionSpec
    syntaxSpec
