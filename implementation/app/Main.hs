module Main
  ( main
  ) where

import Data.Char (isSpace)
import Evaluation (eval)
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
            iterm <- parse tokens
            (fterm, ftype) <- typeCheck iterm
            rterm <- eval fterm
            return (fterm, ftype, rterm)
      case result of
        Left s -> putStrLn ("  " ++ s)
        Right (e, t, r) ->
          putStrLn
            ("  " ++ show e ++ "\n  : " ++ show t ++ "\n  => " ++ show r)

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
