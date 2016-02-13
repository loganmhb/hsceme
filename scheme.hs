module Scheme where

import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.IORef
import Text.ParserCombinators.Parsec

------------
-- Values --
------------

data SchemeVal = Symbol String
               | Pair SchemeVal SchemeVal
               | Number Integer
               | String String
               | Bool Bool
               | Nil
               | PrimitiveFunction ([SchemeVal] -> ThrowsError SchemeVal)
               | Function { params :: [String]
                          , body :: SchemeVal
                          , closure :: Env }

unpackList :: SchemeVal -> [SchemeVal]
unpackList Nil = []
unpackList (Pair h t) = h : unpackList t
unpackList arg = [arg]

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
showVal (Symbol name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal Nil = "()"
showVal (Pair a b) = "(" ++ showVal a ++ showTail b
showVal (PrimitiveFunction f) = "<primitive function>"
showVal (Function params body closure) = "<function>"

instance Show SchemeVal where show = showVal

------------
-- Errors --
------------

data SchemeError = Parser ParseError
                 | Default String
                 | NumArgs Integer [SchemeVal]
                 | TypeError String SchemeVal
                 | UnboundVar String

type ThrowsError = Either SchemeError

showError :: SchemeError -> String
showError (Parser err) = "Parse error at " ++ show err
showError (Default msg) = msg
showError (NumArgs expected got) = "Expected " ++ show expected ++
                                   " arguments but received " ++ (show $ length got)
showError (TypeError t val) = "Value " ++ show val ++ " is not a " ++ t
showError (UnboundVar s) = "Getting an unbound variable: " ++ s

instance Show SchemeError where show = showError

------------------
-- Environments --
------------------

type Env = IORef [(String, IORef SchemeVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT SchemeError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue
                     where trapError errAction = catchError errAction (return . show)
                           extractValue (Right val) = val -- (Left err) will be trapped

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError SchemeVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar var)
        (liftIO . readIORef)
        (lookup var env)


setVar :: Env -> String -> SchemeVal -> IOThrowsError SchemeVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar var)
        (liftIO . (flip writeIORef value))
        (lookup var env)
  return value


defineVar :: Env -> String -> SchemeVal -> IOThrowsError SchemeVal
defineVar envRef var value = do
       alreadyDefined <- liftIO $ isBound envRef var
       if alreadyDefined
         then setVar envRef var value >> return value
         else liftIO $ do
              valueRef <- newIORef value
              env <- readIORef envRef
              writeIORef envRef ((var, valueRef) : env)
              return value


bindVars :: Env -> [(String, SchemeVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)
