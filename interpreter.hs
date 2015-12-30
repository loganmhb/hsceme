module Interpreter where

import Control.Monad.Except
import Scheme

eval :: Env -> SchemeVal -> IOThrowsError SchemeVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Nil) = return val
eval env (Pair (Atom "quote") (Pair form Nil)) = return form
eval env (Pair (Atom func) args) = do
  f <- getVar env func
  evaledArgs <- mapM (eval env) $ unpackList args
  liftThrows $ apply f $ evaledArgs
eval env badForm = throwError $ Default "Unrecognized special form."

apply :: SchemeVal -> [SchemeVal] -> ThrowsError SchemeVal
apply (PrimitiveFunction f) args = f args
apply notFunc _ = throwError $ TypeError "function" notFunc
