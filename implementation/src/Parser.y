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
  , listType
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
  '++'   { TokenPlusPlus }
  ','    { TokenComma }
  '-'    { TokenDash }
  '->'   { TokenArrow }
  '.'    { TokenDot }
  '/'    { TokenSlash }
  ':'    { TokenAnno }
  ';'    { TokenSemicolon }
  '='    { TokenEquals }
  '['    { TokenLSquare }
  ']'    { TokenRSquare }
  X      { TokenIdUpper $$ }
  else   { TokenElse }
  false  { TokenFalse }
  forall { TokenForAll }
  i      { TokenIntLit $$ }
  if     { TokenIf }
  lambda { TokenLambda }
  then   { TokenThen }
  true   { TokenTrue }
  x      { TokenIdLower $$ }

%nonassoc TCONST

%nonassoc ':' ';' '.' else
%right '->'
%left '++'
%left '+' '-'
%left '*' '/'

%nonassoc forall X

%nonassoc i true false if x lambda '(' '['

%nonassoc APP
%nonassoc TAPP

%%

ITerm
  : i                              { IEIntLit $1 }
  | ITerm '+' ITerm                { IEAddInt $1 $3 }
  | ITerm '-' ITerm                { IESubInt $1 $3 }
  | ITerm '*' ITerm                { IEMulInt $1 $3 }
  | ITerm '/' ITerm                { IEDivInt $1 $3 }
  | true                           { IETrue }
  | false                          { IEFalse }
  | if ITerm then ITerm else ITerm { IEIf $2 $4 $6 }
  | '[' EListItems ']'             { IEList (reverse $2) }
  | ITerm '++' ITerm               { IEConcat $1 $3 }
  | x                              { IEVar (EVarName $1) }
  | x '->' ITerm                   { IEAbs (EVarName $1) Nothing $3 }
  | lambda EVarList '->' ITerm     { foldr (\(x, t) e -> IEAbs x t e) $4 (reverse $2) }
  | ITerm ITerm %prec APP          { IEApp $1 $2 }
  | ITerm ':' Type                 { IEAnno (annotate $1 $3) $3 }
  | x '=' ITerm ';' ITerm          { IELet (EVarName $1) $3 $5 }
  | '(' ITerm ')'                  { $2 }

Type
  : '[' Type ']'             { listType $2 }
  | x                        { TVar (UserTVarName $1) }
  | TConst %prec TCONST      { TConst (UserTConstName $ fst $1) (reverse $ snd $1) }
  | Type '->' Type           { TArrow $1 $3 }
  | forall TVarList '.' Type { foldr (\x t -> TForAll x t) $4 (reverse $2) }
  | '(' Type ')'             { $2 }

EListItems
  :                      { [] }
  | ITerm                { [$1] }
  | EListItems ',' ITerm { $3 : $1 }

EVar
  : x                  { (EVarName $1, Nothing) }
  | '(' x ':' Type ')' { (EVarName $2, Just $4) }

TVar
  : x { UserTVarName $1 }

EVarList
  : EVar          { [$1] }
  | EVarList EVar { $2 : $1 }

TVarList
  : TVar          { [$1] }
  | TVarList TVar { $2 : $1 }

TConst
  : X                      { ($1, []) }
  | TConst Type %prec TAPP { (fst $1, $2 : snd $1) }

{

parseError :: [Token] -> Either String a
parseError x = Left $ "Cannot parse: " ++ unwords (show <$> x)

}
