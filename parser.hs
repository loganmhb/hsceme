module Parser where

import Scheme
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapeChar :: Parser Char
escapeChar = char '\\' >> (oneOf "\\\"ntr")

nonClosingQuote :: Parser Char
nonClosingQuote = (noneOf "\"") <|> escapeChar

parseString :: Parser SchemeVal
parseString = do
  char '"'
  x <- many nonClosingQuote
  char '"'
  return $ String x

parseAtom :: Parser SchemeVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser SchemeVal
parseNumber = many1 digit >>= (return . Number . read)

parseQuoted :: Parser SchemeVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ constructList [Atom "quote", x]

parseList :: Parser SchemeVal
parseList = liftM constructList $ sepBy parseExpr spaces

parseDottedList :: Parser SchemeVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ constructImproperList $ head ++ [tail]

parseExpr :: Parser SchemeVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> ThrowsError SchemeVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val
