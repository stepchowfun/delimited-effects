module Evaluation
  ( eval
  ) where

import Control.Monad.Except (throwError)
import Syntax (FTerm(..), substEVarInFTerm, substTVarInFTerm)

eval :: FTerm -> Either String FTerm
eval (FEVar x) = throwError $ "Unbound variable: " ++ show x
eval (FEAbs x t e) = return $ FEAbs x t e
eval (FEApp e1 e2) = do
  e3 <- eval e1
  e4 <- eval e2
  case e3 of
    FEAbs x _ e5 -> return $ substEVarInFTerm x e4 e5
    _ ->
      throwError $ "Type error: cannot apply " ++ show e3 ++ " to " ++ show e4
eval (FETAbs a e) = return $ FETAbs a e
eval (FETApp e1 t) = do
  e2 <- eval e1
  case e2 of
    FETAbs a e3 -> return $ substTVarInFTerm a t e3
    _ ->
      throwError $ "Type error: cannot apply " ++ show e2 ++ " to " ++ show t
