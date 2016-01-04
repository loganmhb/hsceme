module Primitives where

import Scheme
import Control.Monad.Except

primitives :: [(String, [SchemeVal] -> ThrowsError SchemeVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("string?", oneAryPred isString),
              ("symbol?", oneAryPred isSymbol),
              ("atom?", oneAryPred isAtom),
              ("pair?", oneAryPred isPair),
              ("number?", oneAryPred isNumber)]

oneAryPred :: (SchemeVal -> Bool) -> [SchemeVal] -> ThrowsError SchemeVal
oneAryPred p (args : []) = return $ Bool $ p args
oneAryPred _ args = throwError $ NumArgs 1 args

isSymbol :: SchemeVal -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

isString :: SchemeVal -> Bool
isString (String _) = True
isString _ = False

isAtom :: SchemeVal -> Bool -- i.e. is not pair
isAtom Nil = False
isAtom (Pair _ _ ) = False
isAtom _ = True

isPair :: SchemeVal -> Bool
isPair (Pair _ _) = True
isPair Nil = True
isPair _ = False

isNumber :: SchemeVal -> Bool
isNumber (Number _) = True
isNumber _ = False

unpackNum :: SchemeVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNumber = throwError $ TypeError "number" notNumber

numericBinop :: (Integer -> Integer -> Integer) -> [SchemeVal] -> ThrowsError SchemeVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op
