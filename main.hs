module Main where

import System.Environment
import Parser

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ case readExpr (unwords args) of
    (Left err) -> show err
    (Right val) -> show val
