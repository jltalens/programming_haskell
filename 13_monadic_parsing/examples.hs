module Examples where

import Control.Applicative
import Data.Char

-- Basic definitions
newtype Parser a = P (String -> [(a, String)])

-- Removing the dummy constructor
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\s -> case s of
                  [] -> []
                  (x:xs) -> [(x,xs)]
         )

-- Sequencing parsers

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P (\s -> case parse p s of
                   [] -> []
                   [(v, s')] -> [((f v), s')])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P(\s -> [(v, s)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  -- parse pf s => [(a -> b), s]
  -- fmap (a -> b) Parser a => Parser b
  pf <*> px = P (\s -> case parse pf s of
                    [] -> []
                    [(g, s')] -> parse (fmap g px) s')

three :: Parser (Char,Char)
three = pure g <*> item <*> item <*> item
        where g x _ z = (x,z)


instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  -- parse (Parser v) s => [(v, s)]
  p >>= f = P(\s -> case parse p s of
                 [] -> []
                 [(v, s')] -> parse (f v) s')

three' :: Parser (Char, Char)
three' = do x <- item
            _ <- item
            z <- item
            return (x,z)

-- Making choices
{--
How to choose operator in case of failure for an Applicative Parser. Enter Alternative
From Control.Applicative:
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

"empty" represents an alternative that has failed and <|> is appropiate choice operator for this type

Alternative laws:
  empty <|> x = x
  x <|> empty = x
  x <|> (y <|> z) = (x <|> y) <|> z
--}

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\_ -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\s -> case parse p s of
                 [] -> parse q s
                 [(v, s')] -> [(v, s')])

-- Derived primitives

-- Parser sat p for single characters that satisfy p
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

-- We can now define parsers for single digits, etc.
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
string [] = return []
string (x:xs) = do _ <- char x
                   _ <- string xs
                   return (x:xs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do _ <- many (sat isSpace)
           return ()

int :: Parser Int
int = do _ <- char '-'
         n <- nat
         return (-n)
      <|> nat

-- Handling spacing
token :: Parser a -> Parser a
token p = do _ <- space
             v <- p
             _ <- space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do _ <- symbol "["
          n <- natural
          ns <- many (do _ <- symbol ","; natural)
          _ <- symbol "]"
          return (n:ns)

-- Arithmetic expressions
expr :: Parser Int
expr = do t <- term
          do _ <- symbol "+"
             e <- expr
             return (t + e)
             <|> return t

term :: Parser Int
term = do f <- factor
          do _ <- symbol "*"
             t <- term
             return (f * t)
             <|> return f

factor :: Parser Int
factor = do _ <- symbol "("
            e <- expr
            _ <- symbol ")"
            return e
         <|> natural

eval :: String -> Int
eval xs = case (parse expr xs) of
            [(n, [])] -> n
            [(_, s)] -> error ("Unused input " ++ s)
            [] -> error "Invalid input"
