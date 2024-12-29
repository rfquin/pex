module Compiler where

import Data.Char
import Control.Applicative
import Data.Maybe

newtype Parser a = Parser (String -> Maybe (String, a))

-- Parser definition
parse :: Parser a -> String -> Maybe (String,a)
parse (Parser a)= a

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative Parser where
  pure x = Parser $ \s -> Just (s,x)
  Parser f <*> Parser x =
    Parser $ \s ->
    do
      (s',f') <- f s
      fmap f' <$> x s'

instance Monad Parser where
  return = pure
  Parser p >>= f =
    Parser $ \s ->
    do
      (s',a) <- p s
      let Parser p' = f a
      p' s'

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 =
    Parser $ \s -> p1 s <|> p2 s

-- Data structures

data Operator = Add | Mul | Div | Sub deriving Show
data AlgebraicObj = Number Int | AlgebraicObj Operation deriving Show
data Operation = Operation {first :: AlgebraicObj, second :: AlgebraicObj, operator :: Operator} deriving Show

-- parseOperation :: Parser Int
parseOperation :: Parser Operation
parseOperation = do
  _ <- eatEmpty
  op <- parseOperator
  (a, b) <- parseParam
  return $ Operation (Number a) (Number b) op

parseOperator :: Parser Operator
parseOperator = Parser $ \s -> case take 4 s of
  "\\add" -> Just (drop 4 s, Add)
  "\\mul" -> Just (drop 4 s, Mul)
  "\\div" -> Just (drop 4 s, Div)
  "\\sub" -> Just (drop 4 s, Sub)
  _ -> Nothing

eatEmpty :: Parser String
eatEmpty = eatChar $ parseChar ' '

parseParam :: Parser (Int,Int)
parseParam = do
  _ <- parseChar '{'
  a <- parseInt
  _ <- parseChar ','
  b <- parseInt
  _ <- parseChar '}'
  return (a,b)

eatChar :: Parser Char -> Parser String 
eatChar f = Parser $ \s -> case parse f s of
  Just (rest, _) -> parse (eatChar f) rest
  Nothing -> Just (s,"")

parseString :: String -> Parser String
parseString = traverse parseChar

parseChar :: Char -> Parser Char
parseChar p = Parser $ \s -> case s of
  (x:xs) -> if (x == p) then Just (xs,p) else Nothing
  _ -> Nothing

parseInt :: Parser Int
parseInt = Parser $ \s ->
  case s of
    (x:_) | isDigit x ->
      case parse parseDigits s of
        Just (rest, digits) -> Just (rest, read digits)
        Nothing -> Nothing
    _ -> Nothing

parseDigits :: Parser String
parseDigits = Parser $ \s -> let (dig, rest) = span isDigit s in Just (rest, dig)

compiler :: IO()
compiler = do
  print "hello"
