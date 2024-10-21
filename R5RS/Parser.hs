module R5RS.Parser
  ( LispVal (Atom, Bool, Number, Quoted, DottedList, String, List),
    parseExpr,
  )
where

import Control.Monad (liftM)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec
  ( Parser,
    char,
    digit,
    endBy,
    letter,
    many,
    many1,
    noneOf,
    oneOf,
    parse,
    sepBy,
    skipMany1,
    space,
    try,
    (<|>),
  )

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"" <|> (char '\\' >> oneOf "\\\"0nrvtbf"))
  char '"'
  return $ String x

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"

readAtom :: String -> String
readAtom input = case parse parseAtom "atom" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"

readCheck :: Parser a -> String -> String
readCheck p input = case parse p "test" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  Quoted <$> parseExpr

data LispVal
  = Atom String
  | Number Integer
  | Bool Bool
  | String String
  | Quoted LispVal
  | List [LispVal]
  | DottedList [LispVal] LispVal

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseNumber
    <|> parseString
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x