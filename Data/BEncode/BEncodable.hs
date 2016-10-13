{-# LANGUaGE TypeFamilies #-}
{-# LANGUaGE FlexibleInstances #-}

module Data.BEncode.BEncodable
( BEncodable(..)
, bencode
) where

import Data.BEncode.Type

class BEncodable a where
  data Builder a :: *
  collect     :: [Builder a] -> Builder a
  inject      :: a -> Builder a
  injectLen   :: a -> Builder a
  injectInt   :: Integer -> Builder a
  build       :: Builder a -> a
  beginString :: Builder a
  beginInt    :: Builder a
  beginList   :: Builder a
  beginDict   :: Builder a
  end         :: Builder a

instance BEncodable [Char] where
  newtype Builder [Char] = MkStringBuilder ShowS
  collect = foldr (\(MkStringBuilder x) (MkStringBuilder y) ->
                      MkStringBuilder (x . y))
                  (MkStringBuilder id)
  inject = MkStringBuilder . showString
  injectLen = MkStringBuilder . shows . length
  injectInt = MkStringBuilder . shows
  build (MkStringBuilder x) = x ""
  beginString = MkStringBuilder $ showString ":"
  beginInt    = MkStringBuilder $ showString "i"
  beginList   = MkStringBuilder $ showString "l"
  beginDict   = MkStringBuilder $ showString "d"
  end         = MkStringBuilder $ showString "e"

bencode :: BEncodable a => BEncode a -> a
bencode = build . bbuild

bbuild :: BEncodable a => BEncode a -> Builder a
bbuild (BInt i) = collect [beginInt, injectInt i, end]
bbuild (BList xs) = collect $ (beginList : map bbuild xs) ++ [end]
bbuild (BString s) = bbuildString s
bbuild (BDict m) = collect $ (beginDict : map bbuildPair m) ++ [end]

bbuildPair :: BEncodable a => (a, BEncode a) -> Builder a
bbuildPair (s, x) = collect $ [bbuildString s, bbuild x]

bbuildString :: BEncodable a => a -> Builder a
bbuildString s = collect $ [injectLen s, beginString, inject s]
