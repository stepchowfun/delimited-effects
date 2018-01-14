{
module Lexer (alexScanTokens, Token(..)) where
}

%wrapper "basic"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters

tokens :-
  $white+                         ;
  "--".*                          ;
  true                            { \_ -> TokenTrue }
  false                           { \_ -> TokenFalse }
  if                              { \_ -> TokenIf }
  then                            { \_ -> TokenThen }
  else                            { \_ -> TokenElse }
  [\\]                            { \_ -> TokenAbs }
  "->"                            { \_ -> TokenArrow }
  handle                          { \_ -> TokenHandle }
  with                            { \_ -> TokenWith }
  in                              { \_ -> TokenIn }
  bool                            { \_ -> TokenBool }
  $alpha [$alpha $digit \_ \']*   { \s -> TokenVar s }
  [\:]                            { \_ -> TokenAnno }
  [\(]                            { \_ -> TokenLParen }
  [\)]                            { \_ -> TokenRParen }

{
data Token
  = TokenTrue
  | TokenFalse
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenAbs
  | TokenArrow
  | TokenHandle
  | TokenWith
  | TokenIn
  | TokenBool
  | TokenVar String
  | TokenAnno
  | TokenLParen
  | TokenRParen
  deriving (Eq, Show)
}
