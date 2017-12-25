module Main (main) where

import Lexer as Lexer
import Lib ()
import Parser as Parser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      program <- readFile file
      let tokens = Lexer.alexScanTokens program
      print tokens
      print (Parser.parse tokens)
    _ -> print "Provide the name of a file containing \
               \ a program to execute. For example, `implementation-exe test`."
