module Scheme where

import Control.Monad.Except
import Text.ParserCombinators.Parsec

data SchemeVal = Atom String
               | Pair SchemeVal SchemeVal
               | Number Integer
               | String String
               | Bool Bool
               | Nil

constructList :: [SchemeVal] -> SchemeVal
constructList [] = Nil
constructList (car:[]) = Pair car Nil
constructList (car:cdr) = Pair car (constructList cdr)

-- This function shouldn't be called with a list of one or zero elements
constructImproperList :: [SchemeVal] -> SchemeVal
constructImproperList (a:b:[]) = Pair a b
constructImproperList (a:b:cs) = Pair a (constructImproperList (b:cs))

showTail :: SchemeVal -> String
showTail (Pair val Nil) = " " ++ showVal val ++ ")"
showTail (Pair val tailVal) = " " ++ showVal val ++ showTail tailVal
showTail val = " . " ++ showVal val ++ ")"

showVal :: SchemeVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal Nil = "()"
showVal (Pair a b) = "(" ++ showVal a ++ showTail b

instance Show SchemeVal where show = showVal

data SchemeError = Parser ParseError
                 | Default String

type ThrowsError = Either SchemeError

showError :: SchemeError -> String
showError (Parser err) = "Parse error at " ++ show err
showError (Default msg) = msg

instance Show SchemeError where show = showError
