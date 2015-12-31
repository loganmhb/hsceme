module Interpreter where

import Prelude hiding (pred)
import Control.Monad.Except
import Scheme

eval :: Env -> SchemeVal -> IOThrowsError SchemeVal
-- Primitives
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Nil) = return val
-- QUOTE special form
eval env (Pair (Atom "quote") (Pair form Nil)) = return form
-- IF special form
eval env (Pair (Atom "if") (Pair pred (Pair conseq (Pair alt Nil)))) = do
   evaledPred <- (eval env pred)
   case evaledPred of
     Bool False -> eval env alt
     Nil -> eval env alt
     otherwise -> eval env conseq
eval env (Pair (Atom "if") (Pair pred (Pair conseq Nil))) = do
  evaledPred <- (eval env pred)
  case evaledPred of
    Bool False -> return Nil
    Nil -> return Nil
    otherwise -> eval env conseq
eval env (Pair (Atom func) args) = do
  f <- getVar env func
  evaledArgs <- mapM (eval env) $ unpackList args
  liftThrows $ apply f $ evaledArgs
eval env badForm = throwError $ Default "Unrecognized special form."

apply :: SchemeVal -> [SchemeVal] -> ThrowsError SchemeVal
apply (PrimitiveFunction f) args = f args
apply notFunc _ = throwError $ TypeError "function" notFunc
