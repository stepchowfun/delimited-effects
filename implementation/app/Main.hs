module Main
  ( main
  ) where

import Data.Char (isSpace)
import Inference (typeCheck)
import Lexer (scan)
import Parser (parse)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

runProgram :: String -> IO ()
runProgram program =
  if all isSpace program
    then return ()
    else do
      let result = do
            tokens <- scan program
            term <- parse tokens
            typeCheck term
      case result of
        Left s -> putStrLn ("  " ++ s)
        Right (e, t) -> putStrLn ("  " ++ show e ++ "\n  : " ++ show t)

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
      in repl
    _ -> putStrLn "Usage:\n  implementation-exe\n  implementation-exe <path>"
