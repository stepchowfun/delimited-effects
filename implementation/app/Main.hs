module Main (main) where

import Inference (infer)
import Lexer (alexScanTokens)
import Parser (parse)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      program <- readFile file
      let tokens = alexScanTokens program
      let term   = parse tokens
      let fterm  = infer term
      putStrLn "Parsed term:"
      putStrLn ("  " ++ show term)
      putStrLn "Inferred term:"
      putStrLn ("  " ++ show fterm)
    _ -> putStrLn "Usage:\n  implementation-exe <path>"
