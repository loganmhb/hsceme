module Main where

import System.Environment
import System.IO
import Control.Monad
import Parser
import Interpreter
import Primitives
import Scheme

primitiveEnv :: IO Env
primitiveEnv = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
               where makePrimitiveFunc (var, func) = (var, PrimitiveFunction func)

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

runRepl :: IO ()
runRepl = primitiveEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = runRepl
