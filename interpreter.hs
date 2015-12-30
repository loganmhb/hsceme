module Interpreter where

import Scheme

eval :: SchemeVal -> SchemeVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Nil) = val
eval (Pair (Atom "quote") (Pair form Nil)) = form
