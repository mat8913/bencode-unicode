{-# LANGUaGE FlexibleInstances #-}

module Data.BEncode.BDecodable
( BDecodable(..)
, bdecode
) where

import Prelude hiding (null, splitAt)

import Control.Applicative (Alternative(..))
import Control.Monad (ap, guard)
import Data.List (foldl')
import qualified Data.List as List

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C8L

import Data.BEncode.Type
import Data.BEncode.IntConv

class BDecodable a where
  uncons   :: a -> Maybe (Char, a)
  splitAt  :: Integer -> a -> Maybe (a, a)
  null     :: a -> Bool

instance BDecodable [Char] where
  uncons = List.uncons
  splitAt n ls
    | n <  0    = Nothing
    | otherwise = splitAt' n ls
      where
          splitAt' :: Integer -> [Char] -> Maybe ([Char], [Char])
          splitAt' 0  xs     = Just ([], xs)
          splitAt' _  []     = Nothing
          splitAt' m  (x:xs) = do
            (xs', xs'') <- splitAt' (m - 1) xs
            return (x:xs', xs'')

  null = List.null

instance BDecodable Text where
  uncons = Text.uncons
  splitAt n s = do
    n' <- integerToInt n
    let (x, xs) = Text.splitAt n' s
    guard (Text.compareLength x n' == EQ)
    return (x, xs)
  null = Text.null

instance BDecodable TextL.Text where
  uncons = TextL.uncons
  splitAt n s = do
    n' <- integerToInt64 n
    let (x, xs) = TextL.splitAt n' s
    guard (TextL.compareLength x n' == EQ)
    return (x, xs)
  null = TextL.null

instance BDecodable ByteString where
  uncons = C8.uncons
  splitAt n s = do
    n' <- integerToInt n
    let (x, xs) = BS.splitAt n' s
    guard (BS.length x == n')
    return (x, xs)
  null = BS.null

instance BDecodable BSL.ByteString where
  uncons = C8L.uncons
  splitAt n s = do
    n' <- integerToInt64 n
    let (x, xs) = BSL.splitAt n' s
    guard (BSL.length x == n')
    return (x, xs)
  null = BSL.null

bdecode :: BDecodable a => a -> Maybe (BEncode a)
bdecode = fmap fst . runParser (parseOnly bparse)

bparse :: BDecodable a => Parser a (BEncode a)
bparse =     fmap BInt parseBInt
         <|> fmap BList parseBList
         <|> fmap BDict parseBDict
         <|> fmap BString parseBString

parseBInt :: BDecodable a => Parser a Integer
parseBInt = do
    matchChar 'i'
    i <- parseInteger
    matchChar 'e'
    return i

parseBList :: BDecodable a => Parser a [BEncode a]
parseBList = do
    matchChar 'l'
    xs <- many bparse
    matchChar 'e'
    return xs

parseBDict :: BDecodable a => Parser a [(a, BEncode a)]
parseBDict = do
    matchChar 'd'
    xs <- many $ do
        key <- parseBString
        val <- bparse
        return (key, val)
    matchChar 'e'
    return xs

parseBString :: BDecodable a => Parser a a
parseBString = do
    len <- parseUnsignedInteger True
    matchChar ':'
    nextString len

end :: BDecodable str => Parser str ()
end = MkParser $ \s -> if null s then Just ((), s) else Nothing

parseOnly :: BDecodable str => Parser str a -> Parser str a
parseOnly p = do
    x <- p
    end
    return x

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

optional :: Parser str a -> Parser str (Maybe a)
optional (MkParser x) = MkParser $ \s -> Just $ case x s of
  Nothing -> (Nothing, s)
  Just (x', s') -> (Just x', s')

nextChar :: BDecodable str => Parser str Char
nextChar = MkParser uncons

nextString :: BDecodable str => Integer -> Parser str str
nextString = MkParser . splitAt

matchChar :: BDecodable str => Char -> Parser str ()
matchChar c = do
    x <- nextChar
    guard (x == c)

nextDigit :: BDecodable str => Parser str Integer
nextDigit = do
  c <- nextChar
  case digitToInt c of
    Nothing -> empty
    Just d  -> return d

getDigits :: BDecodable str => Parser str [Integer]
getDigits = some nextDigit

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
