module Evaluation
  ( eval
  ) where

import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Syntax (FTerm(..), subst)

eval :: FTerm -> Either String FTerm
eval (FEVar x) = throwError $ "Unbound variable: " ++ show x
eval (FEAbs x t e) = return $ FEAbs x t e
eval (FEApp e1 e2) = do
  e3 <- eval e1
  e4 <- eval e2
  case e3 of
    FEAbs x _ e5 -> eval $ subst x e4 e5
    _ -> throwError $ "Cannot apply " ++ show e3 ++ " to " ++ show e4
eval (FETAbs a k e) = return $ FETAbs a k e
eval (FETApp e1 t) = do
  e2 <- eval e1
  case e2 of
    FETAbs a _ e3 -> eval $ subst a t e3
    _ -> throwError $ "Cannot apply " ++ show e2 ++ " to " ++ show t
eval FETrue = return FETrue
eval FEFalse = return FEFalse
eval (FEIf e1 e2 e3) = do
  e4 <- eval e1
  e5 <- eval e2
  e6 <- eval e3
  case e4 of
    FETrue -> return e5
    FEFalse -> return e6
    _ -> throwError $ show e4 ++ " is not a Boolean"
eval (FEIntLit i) = return $ FEIntLit i
eval (FEAdd e1 e2) = do
  e3 <- eval e1
  e4 <- eval e2
  case (e3, e4) of
    (FEIntLit i1, FEIntLit i2) -> return $ FEIntLit (i1 + i2)
    _ -> throwError $ "Cannot add " ++ show e3 ++ " and " ++ show e4
eval (FESub e1 e2) = do
  e3 <- eval e1
  e4 <- eval e2
  case (e3, e4) of
    (FEIntLit i1, FEIntLit i2) -> return $ FEIntLit (i1 - i2)
    _ -> throwError $ "Cannot subtract " ++ show e4 ++ " from " ++ show e3
eval (FEMul e1 e2) = do
  e3 <- eval e1
  e4 <- eval e2
  case (e3, e4) of
    (FEIntLit i1, FEIntLit i2) -> return $ FEIntLit (i1 * i2)
    _ -> throwError $ "Cannot multiply " ++ show e3 ++ " and " ++ show e4
eval (FEDiv e1 e2) = do
  e3 <- eval e1
  e4 <- eval e2
  case (e3, e4) of
    (FEIntLit i1, FEIntLit i2) ->
      if i2 == 0
        then throwError $ "Cannot divide " ++ show i1 ++ " by 0"
        else return $ FEIntLit (i1 `div` i2)
    _ -> throwError $ "Cannot divide " ++ show e3 ++ " by " ++ show e4
eval (FEList es1) = do
  es2 <-
    foldM
      (\es2 e1 -> do
         e2 <- eval e1
         return $ e2 : es2)
      []
      es1
  return $ FEList $ reverse es2
eval (FEConcat e1 e2) = do
  e3 <- eval e1
  e4 <- eval e2
  case (e3, e4) of
    (FEList es1, FEList es2) -> return $ FEList (es1 ++ es2)
    _ -> throwError $ "Cannot concatenate " ++ show e3 ++ " and " ++ show e4
