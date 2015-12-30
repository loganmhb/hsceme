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
              ("remainder", numericBinop rem)]

unpackNum :: SchemeVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNumber = throwError $ TypeError "number" notNumber

numericBinop :: (Integer -> Integer -> Integer) -> [SchemeVal] -> ThrowsError SchemeVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op
