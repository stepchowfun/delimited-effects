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

runProgram :: String -> IO ()
runProgram program =
  if all isSpace program
    then return ()
    else do
      result <-
        runExceptT $ do
          tokens <- ExceptT . return $ scan program
          iterm <- ExceptT . return $ parse tokens
          lift . putStrLn $ "    Parsed term: " ++ show iterm
          (fterm, ftype) <- ExceptT . return $ typeCheck iterm
          lift . putStrLn $ "  Inferred term: " ++ show fterm
          lift . putStrLn $ "  Inferred type: " ++ show ftype
          rterm <- ExceptT . return $ eval fterm
          lift . putStrLn $ "         Result: " ++ show rterm
          return ()
      case result of
        Left s -> putStrLn ("          Error: " ++ s)
        Right () -> return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      program <- readFile file
      runProgram program
    [] ->
      let repl = do
            input <- readline "> "
            case input of
              Just program -> do
                addHistory program
                runProgram program
                repl
              Nothing -> return ()
      in repl
    _ -> putStrLn "Usage:\n  implementation-exe\n  implementation-exe <path>"
