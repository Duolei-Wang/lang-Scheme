{-# OPTIONS_GHC -Wno-deprecations #-}

module R5RS.Parser where

import Control.Monad (liftM)
import Control.Monad.Error
  ( Error,
    MonadError (throwError),
    MonadIO (liftIO),
    noMsg,
    strMsg,
  )
import Control.Monad.Trans.Error (ErrorT)
import Data.IORef (IORef, newIORef, readIORef)
import System.Environment (getArgs)
import Text.Parsec (ParseError)
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

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var =
  do
    env <- liftIO $ readIORef envRef
    maybe
      (throwError $ UnboundVar "Getting an unbound variable" var)
      (liftIO . readIORef)
      (lookup var env)

instance Error LispError where
  noMsg :: LispError
  noMsg = Default "An error has occurred"
  strMsg :: String -> LispError
  strMsg = Default

type ThrowsError = Either LispError

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

data LispVal
  = Atom String
  | Number Integer
  | Bool Bool
  | String String
  | Quoted LispVal
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func
      { params :: [String],
        vararg :: Maybe String,
        body :: [LispVal],
        closure :: Env
      }

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