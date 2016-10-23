module Data.BEncode.BEncodable
( BEncodable(..)
, bencode
) where

import Data.BEncode.Type
import Data.BEncode.Class


{-# SPECIALIZE bencode :: BEncode String -> String #-}
bencode :: BEncodable a b => BEncode a -> a
bencode = build . bbuild

bbuild :: BEncodable a b => BEncode a -> b
bbuild (BInt i) = collect [beginInt, injectInt i, end]
bbuild (BList xs) = collect $ (beginList : map bbuild xs) ++ [end]
bbuild (BString s) = bbuildString s
bbuild (BDict m) = collect $ (beginDict : map bbuildPair m) ++ [end]

bbuildPair :: BEncodable a b => (a, BEncode a) -> b
bbuildPair (s, x) = collect $ [bbuildString s, bbuild x]

bbuildString :: BEncodable a b => a -> b
bbuildString s = collect [injectLen s, beginString, inject s]
