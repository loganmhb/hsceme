module Interpreter where

import Prelude hiding (pred)
import Control.Monad.Except
import Scheme
import Primitives
import Data.IORef


----------
-- eval --
----------

eval :: Env -> SchemeVal -> IOThrowsError SchemeVal

-- Primitives --
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Nil) = return val

-- Special forms --

-- - QUOTE
eval env (Pair (Symbol "quote") (Pair form Nil)) = return form

-- - IF
eval env (Pair (Symbol "if") (Pair pred (Pair conseq alt))) = do
  evaledPred <- (eval env pred)
  case evaledPred of
    Bool False -> evaledAlt
    Nil -> evaledAlt
    _ -> eval env conseq -- FIXME: Does not error on '(if #t 1 2 3)'
    where evaledAlt = case alt of
            (Pair expr Nil) -> eval env expr
            (Bool False) -> return Nil
            Nil -> return Nil
            _ -> throwError $ Default "Badly formed arguments to IF"

-- - LAMBDA
eval env (Pair (Symbol "lambda") (Pair args body)) =
  case args of
    a@(Pair _ _)
     | (all isSymbol $ unpackList a) -> return $ Function (unpackSymbolList args) body env
     | otherwise -> throwError $ Default "All parameters must be symbols."
    _ -> throwError $ TypeError "Parameters must be a list." args
  where unpackSymbolList = (map unpackSymbol) . unpackList
        unpackSymbol (Symbol s) = s

-- Symbol resolution
eval env (Symbol s) = getVar env s

-- Function application --
eval env (Pair f args) = do
  func <- eval env f
  evaledArgs <- mapM (eval env) $ unpackList args
  apply func $ evaledArgs

-- Base case --
eval env badForm = throwError $ Default "Unrecognized special form."


-----------
-- apply --
-----------

apply :: SchemeVal -> [SchemeVal] -> IOThrowsError SchemeVal
apply (PrimitiveFunction f) args = liftThrows $ f args
apply (Function params body closure) args =
  if num params /= num args
     then throwError $ NumArgs (num params) args
     else (liftIO $ bindVars closure $ zip params args) >>= evalBody
  where num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) $ unpackList body
apply notFunc _ = throwError $ TypeError "function" notFunc
