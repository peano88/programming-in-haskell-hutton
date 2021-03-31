{-# LANGUAGE LambdaCase #-}
module Parser where

import           Control.Applicative
import           Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

item :: Parser Char
item = P
  (\case
    []       -> []
    (x : xs) -> [(x, xs)]
  )

instance Functor Parser where
    --fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P
    (\inp -> case parse p inp of
      []         -> []
      [(v, out)] -> [(f v, out)]
    )

instance Applicative Parser where
    -- pure
  pure a = P (\inp -> [(a, inp)])
  -- <*>
  pf <*> px = P
    (\inp -> case parse pf inp of
      []          -> []
      [(f', out)] -> parse (fmap f' px) out
    )

instance Monad Parser where
    -- >>= :: Parser a -> (a -> Parser b) -> Parser b
  px >>= f = P
    (\inp -> case parse px inp of
      []         -> []
      [(a, out)] -> parse (f a) out
    )

instance Alternative Parser where
    -- empty :: Parser a
  empty = P (const [])
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P
    (\inp -> case parse p inp of
      []         -> parse q inp
      [(v, out)] -> [(v, out)]
    )

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []       = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

ident :: Parser String
ident = do
  x  <- lower
  xs <- many alphanum
  return (x : xs)

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

int :: Parser Int
int = do
  char '-'
  n <- nat
  return (-n) <|> nat

token :: Parser a -> Parser a
token p = do
  space
  t <- p
  space
  return t

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do
  symbol "["
  n  <- natural
  ns <- many
    (do
      symbol ","
      natural
    )
  symbol "]"
  return (n : ns)

-- Expression Parser
expr :: Parser Int
expr = do
  t <- term
  do
      symbol "+"
      e <- expr
      return (t + e)
    <|> return t

term :: Parser Int
term = do
  f <- factor
  do
      symbol "*"
      t <- term
      return (f * t)
    <|> return f

factor :: Parser Int
factor =
  do
      symbol "("
      e <- expr
      symbol ")"
      return e
    <|> natural

eval :: String -> Int
eval xs = case parse expr xs of
  [(n, "" )] -> n
  [(_, out)] -> error ("Invalid input: unused: " ++ out)
  []         -> error "invalid input"
