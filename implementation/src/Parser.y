{

module Parser (parse) where

import Lexer (Token(..))
import Lib (Term(..), Type(..))

}

%name parse
%tokentype { Token }
%error { parseError }

%token
  ':'      { TokenAnno }
  '->'     { TokenArrow }
  '.'      { TokenDot }
  forall   { TokenForAll }
  x        { TokenId $$ }
  '('      { TokenLParen }
  lambda   { TokenLambda }
  ')'      { TokenRParen }

%nonassoc ':'
%left '.'
%right '->'
%nonassoc forall x '(' lambda ')'
%nonassoc APP

%%

Term : x                        { EVar $1 }
     | lambda VarList '.' Term  { foldr (\x e -> EAbs x e) $4 (reverse $2) }
     | Term Term %prec APP      { EApp $1 $2 }
     | Term ':' Type            { EAnno $1 $3 }
     | '(' Term ')'             { $2 }

Type : x                        { TVar $1 }
     | Type '->' Type           { TArrow $1 $3 }
     | forall VarList '.' Type  { foldr (\x t -> TForAll x t) $4 (reverse $2) }
     | '(' Type ')'             { $2 }

VarList : x                     { [$1] }
        | VarList x             { $2 : $1 }

{

parseError :: [Token] -> a
parseError x = error ("Parse error: " ++ show x)

}
