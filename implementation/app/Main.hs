module Main (main) where

import Data.Char (isSpace)
import Inference (infer)
import Lexer (scan)
import Parser (parse)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

runProgram :: String -> IO ()
runProgram program = if all isSpace program
  then return ()
  else do
    let result = do
          tokens <- scan program
          term   <- parse tokens
          let fterm = infer term
          return fterm
    case result of
      Left  s     -> putStrLn ("  " ++ s)
      Right fterm -> putStrLn ("  " ++ show fterm)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      program <- readFile file
      runProgram program
    [] ->
      let repl = do
            putStr "> "
            hFlush stdout
            program <- getLine
            runProgram program
            repl
      in  repl
    _ -> putStrLn "Usage:\n  implementation-exe\n  implementation-exe <path>"
