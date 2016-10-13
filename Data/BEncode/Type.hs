module Data.BEncode.Type
( BEncode(..)
) where

data BEncode a = BInt Integer
               | BString a
               | BList [BEncode a]
               | BDict [(a , BEncode a)]
    deriving (Eq, Ord, Read, Show)
