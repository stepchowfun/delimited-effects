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

%nonassoc LOW

%nonassoc ':' ';' '.' else
%right '->'
%left '++'
%left '+' '-'
%left '*' '/'

%nonassoc true false if i x lambda '(' '[' forall X

%nonassoc HIGH

%%

ITerm
  : x                              { IEVar (EVarName $1) }
  | x '->' ITerm                   { IEAbs (EVarName $1) Nothing $3 }
  | lambda EVars '->' ITerm        { foldr (\(x, t) e -> IEAbs x t e) $4 (reverse $2) }
  | ITerm ITerm %prec HIGH         { IEApp $1 $2 }
  | ITerm ':' Type                 { IEAnno (annotate $1 $3) $3 }
  | x '=' ITerm ';' ITerm          { IELet (EVarName $1) $3 $5 }
  | '(' ITerm ')'                  { $2 }
  | true                           { IETrue }
  | false                          { IEFalse }
  | if ITerm then ITerm else ITerm { IEIf $2 $4 $6 }
  | i                              { IEIntLit $1 }
  | ITerm '+' ITerm                { IEAdd $1 $3 }
  | ITerm '-' ITerm                { IESub $1 $3 }
  | ITerm '*' ITerm                { IEMul $1 $3 }
  | ITerm '/' ITerm                { IEDiv $1 $3 }
  | '[' EListItems ']'             { IEList (reverse $2) }
  | ITerm '++' ITerm               { IEConcat $1 $3 }

Type
  : x                     { TVar (UserTVarName $1) }
  | TConst %prec LOW      { TConst (UserTConstName $ fst $1) (reverse $ snd $1) }
  | Type '->' Type        { TArrow $1 $3 }
  | forall TVars '.' Type { foldr (\x t -> TForAll x t) $4 (reverse $2) }
  | '(' Type ')'          { $2 }

EListItems
  :                      { [] }
  | ITerm                { [$1] }
  | EListItems ',' ITerm { $3 : $1 }

EVar
  : x                  { (EVarName $1, Nothing) }
  | '(' x ':' Type ')' { (EVarName $2, Just $4) }

EVars
  : EVar          { [$1] }
  | EVars EVar { $2 : $1 }

TVar
  : x { UserTVarName $1 }

TConst
  : X %prec LOW         { ($1, []) }
  | TConst TVar         { (fst $1, TVar $2 : snd $1) }
  | TConst X            { (fst $1, (TConst (UserTConstName $2) []) : snd $1) }
  | TConst '(' Type ')' { (fst $1, $3 : snd $1) }

TVars
  : TVar          { [$1] }
  | TVars TVar { $2 : $1 }

{

parseError :: [Token] -> Either String a
parseError x = Left $ "Cannot parse: " ++ unwords (show <$> x)

}
