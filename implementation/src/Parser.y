{

module Parser (parse) where

import Lexer (Token(..))
import Syntax
  ( EVarName(..)
  , ITerm(..)
  , TConstName(..)
  , TVarName(..)
  , Type(..)
  , annotate
  , hole
  )

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
  exists { TokenExists }
  forall { TokenForAll }
  i      { TokenIntLit $$ }
  in     { TokenIn }
  lambda { TokenLambda }
  x      { TokenIdLower $$ }
  X      { TokenIdUpper $$ }

%nonassoc ':' '=' in '.'
%right '->'
%left '+' '-'
%left '*' '/'
%nonassoc let exists forall lambda '(' ')' x i
%nonassoc APP

%%

ITerm
  : i                          { IEIntLit $1 }
  | x                          { IEVar (EVarName $1) }
  | x '->' ITerm               { IEAbs (EVarName $1) hole $3 }
  | lambda EVarList '->' ITerm { foldr (\(x, t) e -> IEAbs x t e) $4 (reverse $2) }
  | ITerm ITerm %prec APP      { IEApp $1 $2 }
  | ITerm ':' Type             { IEAnno (annotate $1 $3) $3 }
  | ITerm '+' ITerm            { IEAddInt $1 $3 }
  | ITerm '-' ITerm            { IESubInt $1 $3 }
  | ITerm '*' ITerm            { IEMulInt $1 $3 }
  | ITerm '/' ITerm            { IEDivInt $1 $3 }
  | x '=' ITerm in ITerm       { IELet (EVarName $1) $3 $5 }
  | '(' ITerm ')'              { $2 }

Type
  : x                        { TVar (TVarName $1) }
  | X                        { TConst (TConstName $1) }
  | Type '->' Type           { TArrow $1 $3 }
  | exists TVarList '.' Type { foldr (\x t -> TExists x t) $4 (reverse $2) }
  | forall TVarList '.' Type { foldr (\x t -> TForAll x t) $4 (reverse $2) }
  | '(' Type ')'             { $2 }

EVar
  : x                  { (EVarName $1, hole) }
  | '(' x ':' Type ')' { (EVarName $2, $4) }

TVar
  : x { TVarName $1 }

EVarList
  : EVar          { [$1] }
  | EVarList EVar { $2 : $1 }

TVarList
  : TVar          { [$1] }
  | TVarList TVar { $2 : $1 }

{

parseError :: [Token] -> Either String a
parseError x = Left $ "Parse error: " ++ unwords (show <$> x)

}
