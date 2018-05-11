{

{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Lexer (Token(..), scan) where

}

%wrapper "monad"

$digit = 0-9
$lower = [a-z α-κ]
$upper = A-Z
$idChar = [_ $lower $upper $digit]
@idLower = $lower $idChar*
@idUpper = $upper $idChar*

:-

"#".*          ;
"("            { tokenAtom TokenLParen }
")"            { tokenAtom TokenRParen }
"*"            { tokenAtom TokenAsterisk }
"+"            { tokenAtom TokenPlus }
"++" | "⧺"     { tokenAtom TokenPlusPlus }
","            { tokenAtom TokenComma }
"-"            { tokenAtom TokenDash }
"->" | "→"     { tokenAtom TokenArrow }
"."            { tokenAtom TokenDot }
"/"            { tokenAtom TokenSlash }
":"            { tokenAtom TokenAnno }
";"            { tokenAtom TokenSemicolon }
"="            { tokenAtom TokenEquals }
"["            { tokenAtom TokenLSquare }
"\" | "λ"      { tokenAtom TokenLambda }
"]"            { tokenAtom TokenRSquare }
"else"         { tokenAtom TokenElse }
"false"        { tokenAtom TokenFalse }
"forall" | "∀" { tokenAtom TokenForAll }
"if"           { tokenAtom TokenIf }
"then"         { tokenAtom TokenThen }
"true"         { tokenAtom TokenTrue }
$digit+        { tokenInteger TokenIntLit }
$white+        ;
@idLower       { tokenString TokenIdLower }
@idUpper       { tokenString TokenIdUpper }

{

data Token
  = TokenAnno
  | TokenArrow
  | TokenAsterisk
  | TokenComma
  | TokenDash
  | TokenDot
  | TokenElse
  | TokenEquals
  | TokenFalse
  | TokenForAll
  | TokenIdLower String
  | TokenIdUpper String
  | TokenIf
  | TokenIntLit Integer
  | TokenLParen
  | TokenLSquare
  | TokenLambda
  | TokenPlus
  | TokenPlusPlus
  | TokenRParen
  | TokenRSquare
  | TokenSemicolon
  | TokenSlash
  | TokenThen
  | TokenTrue
  deriving Eq

instance Show Token where
  show (TokenIdLower x) = x
  show (TokenIdUpper c) = c
  show (TokenIntLit x) = show x
  show TokenAnno = ":"
  show TokenArrow = "→"
  show TokenAsterisk = "*"
  show TokenComma = ","
  show TokenDash = "-"
  show TokenDot = "."
  show TokenElse = "else"
  show TokenEquals = "="
  show TokenFalse = "false"
  show TokenForAll = "∀"
  show TokenIf = "if"
  show TokenLParen = "("
  show TokenLSquare = "["
  show TokenLambda = "λ"
  show TokenPlus = "+"
  show TokenPlusPlus = "⧺"
  show TokenRParen = ")"
  show TokenRSquare = "]"
  show TokenSemicolon = ";"
  show TokenSlash = "/"
  show TokenThen = "then"
  show TokenTrue = "true"

alexScanAction :: Alex (Maybe Token)
alexScanAction = do
  input <- alexGetInput
  sc <- alexGetStartCode
  case alexScan input sc of
    AlexEOF -> alexEOF
    AlexError (_, _, _, s) -> alexError $ "Cannot tokenize: " ++ s
    AlexSkip  newInp _ -> do
        alexSetInput newInp
        alexScanAction
    AlexToken newInp len action -> do
        alexSetInput newInp
        action (ignorePendingBytes input) len

scan :: String -> Either String [Token]
scan s = fmap reverse $ runAlex s $ do
  let loop memo = do r <- alexScanAction
                     case r of
                       Nothing -> return memo
                       Just t -> do rest <- loop (t : memo)
                                    return rest
  loop []

alexEOF :: Alex (Maybe Token)
alexEOF = return Nothing

tokenAtom :: Token -> AlexAction (Maybe Token)
tokenAtom t = token (\_ _ -> Just t)

tokenString :: (String -> Token) -> AlexAction (Maybe Token)
tokenString t = token (\(_, _, _, s) len -> Just $ t (take len s))

tokenInteger :: (Integer -> Token) -> AlexAction (Maybe Token)
tokenInteger t = token (\(_, _, _, s) len -> Just $ t (read (take len s) :: Integer))

}
