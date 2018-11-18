module Main
  ( main
  ) where

import Control.Monad.Except (ExceptT(..), lift, runExceptT)
import Data.Char (isSpace)
import Evaluation (eval)
import Inference (typeCheck)
import Lexer (scan)
import Parser (parse)
import System.Console.Readline (addHistory, readline)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

runProgram :: String -> IO Bool
runProgram program =
  if all isSpace program
    then pure True
    else do
      result <-
        runExceptT $ do
          tokens <- ExceptT . pure $ scan program
          iterm <- ExceptT . pure $ parse tokens
          (fterm, ftype) <- ExceptT . pure $ typeCheck iterm
          rterm <- ExceptT . pure $ eval fterm
          lift . putStrLn $ "  ⇒ " ++ show rterm
          lift . putStrLn $ "  : " ++ show ftype
          pure ()
      case result of
        Left s -> do
          putStrLn ("  Error: " ++ s)
          pure False
        Right () -> pure True

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      program <- readFile file
      success <- runProgram program
      if success
        then exitSuccess
        else exitFailure
    [] ->
      let repl = do
            input <- readline "⨠ "
            case input of
              Just program -> do
                addHistory program
                _ <- runProgram program
                repl
              Nothing -> putStrLn ""
       in repl
    _ -> putStrLn "Usage:\n  implementation-exe\n  implementation-exe <path>"
