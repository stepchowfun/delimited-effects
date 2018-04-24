module Main (main) where

import Lexer
import Lib ()
import Parser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      program <- readFile file
      let tokens = Lexer.alexScanTokens program
      print (Parser.parse tokens)
    _ -> putStrLn "Usage:\n  implementation-exe <path>"
