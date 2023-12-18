module Parser where

import Control.Applicative
import Control.Monad (guard)
import Data.Char (isAlpha, isDigit, ord)
import Logic
import Test.QuickCheck
import Text.Read (readMaybe)
import Prelude hiding (filter)

newtype Parser a = P {doParse :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P $ \s -> do
    (c, cs) <- doParse p s
    return (f c, cs)

--------------------- Applicative Parser -------------------
apP :: Parser (t -> a) -> Parser t -> Parser a
apP p1 p2 = P $ \s -> do
  (f, s') <- doParse p1 s
  (x, s'') <- doParse p2 s'
  return (f x, s'')

pureP :: a -> Parser a
pureP x = P $ \s -> Just (x, s)

instance Applicative Parser where
  pure :: a -> Parser a
  pure = pureP
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) = apP

digitChar :: Parser Char
digitChar = satisfy isDigit

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P $ \s -> do
  (c, cs) <- doParse get s
  guard (f c)
  return (c, cs)

get :: Parser Char
get = P $ \s -> case s of
  (c : cs) -> Just (c, cs)
  [] -> Nothing

filter :: (a -> Bool) -> Parser a -> Parser a
filter f p = P $ \s -> do
  (c, cs) <- doParse p s
  guard (f c)
  return (c, cs)

char :: Char -> Parser Char
char c = filter (== c) get

oneDigit :: Parser Int
oneDigit = P $ \s -> case s of
  (c : cs) -> case (readMaybe [c] :: Maybe Int) of
    Just i -> Just (i, cs)
    Nothing -> Nothing
  [] -> Nothing

chooseFirstP :: Parser a -> Parser a -> Parser a
p1 `chooseFirstP` p2 = P $ \s -> doParse p1 s `firstJust` doParse p2 s

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust Nothing y = y

------------------ Alternative Class for Parser -----------------
failP :: Parser a
failP = P $ const Nothing

instance Alternative Parser where
  empty = failP
  (<|>) = chooseFirstP

oneNat :: Parser Int
oneNat = fmap read (some digitChar)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

locationParser :: Parser Location
locationParser = P $ \s -> case s of
  [] -> Nothing
  _ -> case doParse (sepBy oneNat (char ' ')) s of
    Just inner -> case fst inner of
      x : y : ys -> Just ((x, y), "")
      _ -> Nothing
    _ -> Nothing
