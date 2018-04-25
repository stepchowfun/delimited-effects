{

{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Lexer (scan, Token(..)) where

}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]
@identifier = $alpha [$alpha $digit _]*

:-

$white+                         ;
"#".*                           ;
":"                             { tokenAtom TokenAnno }
"->"                            { tokenAtom TokenArrow }
"."                             { tokenAtom TokenDot }
"forall"                        { tokenAtom TokenForAll }
@identifier                     { tokenString TokenId }
"("                             { tokenAtom TokenLParen }
"\"                             { tokenAtom TokenLambda }
")"                             { tokenAtom TokenRParen }

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
  deriving Eq

instance Show Token where
  show TokenAnno = ":"
  show TokenArrow = "->"
  show TokenDot = "."
  show TokenForAll = "forall"
  show (TokenId x) = x
  show TokenLParen = "("
  show TokenLambda = "\\"
  show TokenRParen = ")"

alexScanAction :: Alex (Maybe Token)
alexScanAction = do
  input <- alexGetInput
  sc <- alexGetStartCode
  case alexScan input sc of
    AlexEOF -> alexEOF
    AlexError _ -> alexError $ "Lexical error."
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

}
