module Data.BEncode.Parser
( Parser(..)
, matchChar
, nextString
, parseOnly
, parseInteger
, parseUnsignedInteger
) where

import Prelude hiding (splitAt, null)

import Control.Monad (ap, guard)

import Control.Applicative (Alternative(..))
import Data.List (foldl')

import Data.BEncode.Class (BDecodable(..))


newtype Parser str a = MkParser { runParser :: str -> Maybe (a, str) }

instance Functor (Parser str) where
  fmap f (MkParser x) = MkParser $ \s -> case x s of
    Nothing -> Nothing
    Just (x', s') -> Just (f x', s')

instance Applicative (Parser str) where
  pure x = MkParser (\s -> Just (x, s))
  (<*>) = ap

instance Monad (Parser str) where
  return = pure
  (MkParser x) >>= f = MkParser $ \s -> case x s of
    Nothing -> Nothing
    Just (x', s') -> case f x' of
      (MkParser f') -> f' s'

instance Alternative (Parser str) where
  empty = MkParser $ \_ -> Nothing
  (MkParser x) <|> (MkParser y) = MkParser $ \s -> x s <|> y s

matchChar :: BDecodable str => Char -> Parser str ()
matchChar c = do
    x <- nextChar
    guard (x == c)

nextString :: BDecodable str => Integer -> Parser str str
nextString = MkParser . splitAt

parseOnly :: BDecodable str => Parser str a -> Parser str a
parseOnly p = do
    x <- p
    end
    return x

parseInteger :: BDecodable str => Parser str Integer
parseInteger = do
    minus <- optional $ matchChar '-'
    case minus of
      Nothing -> parseUnsignedInteger True
      Just _  -> fmap negate $ parseUnsignedInteger False

parseUnsignedInteger :: BDecodable str => Bool -> Parser str Integer
parseUnsignedInteger allowZero = do
    digits <- getDigits
    case digits of
      [0]   -> if allowZero then return 0 else empty
      (0:_) -> empty
      _     -> return $ foldl' (\acc x -> acc*10 + x) 0 digits

optional :: Parser str a -> Parser str (Maybe a)
optional (MkParser x) = MkParser $ \s -> Just $ case x s of
  Nothing -> (Nothing, s)
  Just (x', s') -> (Just x', s')

nextChar :: BDecodable str => Parser str Char
nextChar = MkParser uncons

nextDigit :: BDecodable str => Parser str Integer
nextDigit = do
  c <- nextChar
  case digitToInt c of
    Nothing -> empty
    Just d  -> return d

getDigits :: BDecodable str => Parser str [Integer]
getDigits = some nextDigit

end :: BDecodable str => Parser str ()
end = MkParser $ \s -> if null s then Just ((), s) else Nothing

digitToInt :: Char -> Maybe Integer
digitToInt '0' = Just 0
digitToInt '1' = Just 1
digitToInt '2' = Just 2
digitToInt '3' = Just 3
digitToInt '4' = Just 4
digitToInt '5' = Just 5
digitToInt '6' = Just 6
digitToInt '7' = Just 7
digitToInt '8' = Just 8
digitToInt '9' = Just 9
digitToInt _   = Nothing
