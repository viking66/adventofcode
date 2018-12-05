{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative hiding (many)
import Control.Arrow (first)
import Data.Bool (bool)
import Data.Char (isDigit)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser g) = Parser $ (first f <$>) . g

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ Just . ((,) a)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser f <*> Parser g = Parser $ maybe Nothing (\(h, s) -> first h <$> g s) . f

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser f <|> Parser g = Parser $ liftA2 (<|>) f g

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ (\case
                        [] -> Nothing
                        (x:xs) -> bool Nothing (Just (x, xs)) (p x))

char :: Char -> Parser Char
char = satisfy . (==)

many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

many1 :: Parser a -> Parser [a]
many1 p = liftA2 (:) p (many p)

digit :: Parser Char
digit = satisfy isDigit

digits :: Parser [Char]
digits = many digit

number :: Parser Int
number = read <$> digits

space :: Parser Char
space = char ' '

spaces :: Parser [Char]
spaces = many space

sequenceParser :: [Parser a] -> Parser [a]
sequenceParser = foldr (liftA2 (:)) (pure [])

string :: String -> Parser String
string = sequenceParser . map char
