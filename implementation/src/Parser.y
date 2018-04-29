{

module Parser (parse) where

import Lexer (Token(..))
import Syntax (EVar(..), ITerm(..), TVar(..), Type(..))

}

%name parse
%tokentype { Token }
%monad     { Either String }
%error     { parseError }

%token
  '('    { TokenLParen }
  ')'    { TokenRParen }
  '*'    { TokenAsterisk }
  '+'    { TokenPlus }
  '-'    { TokenDash }
  '->'   { TokenArrow }
  '.'    { TokenDot }
  '/'    { TokenSlash }
  ':'    { TokenAnno }
  '='    { TokenEquals }
  forall { TokenForAll }
  i      { TokenIntLit $$ }
  in     { TokenIn }
  lambda { TokenLambda }
  let    { TokenLet }
  x      { TokenId $$ }

%nonassoc ':' '=' in '.'
%right '->'
%left '+' '-'
%left '*' '/'
%nonassoc let forall lambda '(' ')' x i
%nonassoc APP

%%

ITerm
  : i                          { IEIntLit $1 }
  | x                          { IEVar (UserEVar $1) }
  | x '->' ITerm               { IEAbs (UserEVar $1) Nothing $3 }
  | lambda EVarList '->' ITerm { foldr (\(x, t) e -> IEAbs (UserEVar x) t e) $4 (reverse $2) }
  | ITerm ITerm %prec APP      { IEApp $1 $2 }
  | ITerm ':' Type             { IEAnno $1 $3 }
  | ITerm '+' ITerm            { IEAddInt $1 $3 }
  | ITerm '-' ITerm            { IESubInt $1 $3 }
  | ITerm '*' ITerm            { IEMulInt $1 $3 }
  | ITerm '/' ITerm            { IEDivInt $1 $3 }
  | let x '=' ITerm in ITerm   { IELet (UserEVar $2) $4 $6 }
  | '(' ITerm ')'              { $2 }

Type
  : x                        { TVar (UserTVar $1) }
  | Type '->' Type           { TArrow $1 $3 }
  | forall TVarList '.' Type { foldr (\x t -> TForAll (UserTVar x) t) $4 (reverse $2) }
  | '(' Type ')'             { $2 }

EVarList
  : x                           { [($1, Nothing)] }
  | '(' x ':' Type ')'          { [($2, Just $4)] }
  | EVarList x                  { ($2, Nothing) : $1 }
  | EVarList '(' x ':' Type ')' { ($3, Just $5) : $1 }

TVarList
  : x          { [$1] }
  | TVarList x { $2 : $1 }

{

parseError :: [Token] -> Either String a
parseError x = Left ("Parse error: " ++ show x)

}
