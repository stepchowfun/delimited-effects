{
module Parser (parse) where

import Lexer (Token(..))
import Lib (Term(..), Type(..))
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  lambda   { TokenAbs }
  arrow    { TokenArrow }
  handle   { TokenHandle }
  with     { TokenWith }
  in       { TokenIn }
  bool     { TokenBool }
  x        { TokenVar $$ }
  ':'      { TokenAnno }
  '('      { TokenLParen }
  ')'      { TokenRParen }

%%

Term : x                           { EVar $1 }
     | lambda x arrow Term         { EAbs $2 $4 }
     | Term Term                   { EApp $1 $2 }
     | handle x with Term in Term  { EHandle $2 $4 $6 }
     | '(' Term ')'                { $2 }
{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
