module Scheme where

data SchemeVal = Atom String
               | Pair SchemeVal SchemeVal
               | Number Integer
               | String String
               | Bool Bool
               | Nil
