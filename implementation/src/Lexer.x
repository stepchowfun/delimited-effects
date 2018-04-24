{

module Lexer (alexScanTokens, Token(..)) where

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
@identifier = $alpha [$alpha $digit _]*

:-
  $white+                         ;
  "#".*                           ;
  ":"                             { \_ -> TokenAnno }
  "->"                            { \_ -> TokenArrow }
  "."                             { \_ -> TokenDot }
  "forall"                        { \_ -> TokenForAll }
  @identifier                     { \s -> TokenId s }
  "("                             { \_ -> TokenLParen }
  "\"                             { \_ -> TokenLambda }
  ")"                             { \_ -> TokenRParen }

{

data Token
  = TokenAnno
  | TokenArrow
  | TokenDot
  | TokenForAll
  | TokenId String
  | TokenLParen
  | TokenLambda
  | TokenRParen
  deriving (Eq, Show)

}
