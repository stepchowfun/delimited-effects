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
"-"            { tokenAtom TokenDash }
"->" | "→"     { tokenAtom TokenArrow }
"."            { tokenAtom TokenDot }
"/"            { tokenAtom TokenSlash }
":"            { tokenAtom TokenAnno }
"="            { tokenAtom TokenEquals }
"\" | "λ"      { tokenAtom TokenLambda }
"forall" | "∀" { tokenAtom TokenForAll }
"in"           { tokenAtom TokenIn }
$digit+        { tokenInteger TokenIntLit }
$white+        ;
@idLower       { tokenString TokenIdLower }
@idUpper       { tokenString TokenIdUpper }

{

data Token
  = TokenAnno
  | TokenArrow
  | TokenAsterisk
  | TokenDash
  | TokenDot
  | TokenEquals
  | TokenForAll
  | TokenIdLower String
  | TokenIdUpper String
  | TokenIn
  | TokenIntLit Integer
  | TokenLParen
  | TokenLambda
  | TokenPlus
  | TokenRParen
  | TokenSlash
  deriving Eq

instance Show Token where
  show (TokenIdLower x) = x
  show (TokenIdUpper c) = c
  show (TokenIntLit x) = show x
  show TokenAnno = ":"
  show TokenArrow = "→"
  show TokenAsterisk = "*"
  show TokenDash = "-"
  show TokenDot = "."
  show TokenEquals = "="
  show TokenForAll = "∀"
  show TokenIn = "in"
  show TokenLParen = "("
  show TokenLambda = "λ"
  show TokenPlus = "+"
  show TokenRParen = ")"
  show TokenSlash = "/"

alexScanAction :: Alex (Maybe Token)
alexScanAction = do
  input <- alexGetInput
  sc <- alexGetStartCode
  case alexScan input sc of
    AlexEOF -> alexEOF
    AlexError (_, _, _, s) -> alexError $ "Lexical error: " ++ s
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
