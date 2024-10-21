{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module R5RS.Parser where

import Control.Applicative
import Data.List (nub)
import Text.Parsec (Consumed ())

data Error input err
  = EndOfInput
  | Unexpected input
  | CustomError err
  | Empty
  deriving (Eq, Show)

newtype Parser input error output = Parser
  { runParser :: [input] -> Either [Error input error] (output, [input])
  }
  deriving (Functor)

instance Applicative (Parser i e) where
  pure :: a -> Parser i e a
  pure x = Parser $ \i -> Right (x, i)

  (<*>) :: Parser i e (a -> b) -> Parser i e a -> Parser i e b
  (<*>) (Parser pf) (Parser pa) = Parser $ \input ->
    case pf input of
      Left errs -> Left errs
      Right (f, i') -> case pa i' of
        Left errs' -> Left errs'
        Right (a, i'') -> Right (f a, i'')

instance Monad (Parser i e) where
  (>>=) :: Parser i e a -> (a -> Parser i e b) -> Parser i e b
  (>>=) pa f = Parser $ \input -> do
    (out, rest) <- runParser pa input
    (out', rest') <- runParser (f out) rest
    Right (out', rest')

satisfy :: (i -> Bool) -> Parser i e i
satisfy check = Parser $ \input ->
  case input of
    [] -> Left [EndOfInput]
    (x : xs)
      | check x -> Right (x, xs)
      | otherwise -> Left [Unexpected x]

char :: (Eq i) => i -> Parser i e i
char i = satisfy (== i)

string :: (Eq i) => [i] -> Parser i e [i]
string str = case str of
  [] -> pure []
  (x : xs) -> (:) <$> char x <*> string xs

instance (Eq i, Eq e) => Alternative (Parser i e) where
  empty = Parser $ \_ -> Left [Empty]

  (<|>) :: (Eq i, Eq e) => Parser i e a -> Parser i e a -> Parser i e a
  (Parser l) <|> (Parser r) = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)
