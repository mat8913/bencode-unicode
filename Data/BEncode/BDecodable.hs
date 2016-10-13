module Data.BEncode.BDecodable
( BDecodable(..)
, bdecode
) where

import Control.Applicative (Alternative(..))

import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)

import Data.BEncode.Type
import Data.BEncode.Class
import Data.BEncode.Parser


{-# SPECIALIZE bdecode :: String -> Maybe (BEncode String) #-}
{-# SPECIALIZE bdecode :: Text -> Maybe (BEncode Text) #-}
{-# SPECIALIZE bdecode :: Lazy.Text -> Maybe (BEncode Lazy.Text) #-}
{-# SPECIALIZE bdecode :: ByteString -> Maybe (BEncode ByteString) #-}
{-# SPECIALIZE bdecode :: Lazy.ByteString -> Maybe (BEncode Lazy.ByteString) #-}
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
