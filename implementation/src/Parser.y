{

module Parser (parse) where

import Lexer (Token(..))
import Syntax
  ( EVarName(..)
  , ITerm(..)
  , KVarName(..)
  , Kind(..)
  , TConName(..)
  , TVarName(..)
  , Type(..)
  , arrowType
  , listType
  , propagate
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

%nonassoc ';' '.' else
%nonassoc ':'
%right '->'
%left '++'
%left '+' '-'
%left '*' '/'

%nonassoc true false if i x lambda '(' '[' forall X

%nonassoc HIGH

%%

ITerm
  : x                              { IEVar (EVarName $1) }
  | x '->' ITerm                   { IEAbs (EVarName $1) existentialType $3 }
  | lambda EVars '->' ITerm        { foldr (\(x, t) e -> IEAbs x t e) $4 (reverse $2) }
  | ITerm ITerm %prec HIGH         { IEApp $1 $2 }
  | ITerm ':' Type                 { IEAnno (propagate $1 $3) $3 }
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
  | TCon %prec LOW        { TCon (UserTConName $ fst $1) (reverse $ snd $1) }
  | Type '->' Type        { arrowType $1 $3 }
  | forall TVars '.' Type { foldr (\(a, k) t -> TForAll a k t) $4 (reverse $2) }
  | '(' Type ')'          { $2 }

Kind
  : '*' { KType }

EListItems
  :                      { [] }
  | ITerm                { [$1] }
  | EListItems ',' ITerm { $3 : $1 }

EVar
  : x                  { (EVarName $1, existentialType) }
  | '(' x ':' Type ')' { (EVarName $2, $4) }

EVars
  : EVar       { [$1] }
  | EVars EVar { $2 : $1 }

TVar
  : x                  { (UserTVarName $1, existentialKind) }
  | '(' x ':' Kind ')' { (UserTVarName $2, $4) }

TCon
  : X %prec LOW       { ($1, []) }
  | TCon x            { (fst $1, TVar (UserTVarName $2) : snd $1) }
  | TCon X            { (fst $1, (TCon (UserTConName $2) []) : snd $1) }
  | TCon '(' Type ')' { (fst $1, $3 : snd $1) }

TVars
  : TVar       { [$1] }
  | TVars TVar { $2 : $1 }

{

existentialType :: Type
existentialType = TVar $ UserTVarName "α"

existentialKind :: Kind
existentialKind = KVar $ UserKVarName "ω"

parseError :: [Token] -> Either String a
parseError x = Left $ "Cannot parse: " ++ unwords (show <$> x)

}
