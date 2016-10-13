{-# LANGUaGE FlexibleInstances #-}
{-# LANGUaGE TypeFamilies #-}

module Data.BEncode.Class
( BEncodable(..)
, BDecodable(..)
) where

import Prelude hiding (null, splitAt)

import Control.Monad (guard)
import qualified Data.List as List

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C8L

import Data.BEncode.IntConv


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
  {-# INLINE collect #-}
  {-# INLINE inject #-}
  {-# INLINE injectLen #-}
  {-# INLINE injectInt #-}
  {-# INLINE build #-}
  {-# INLINE beginString #-}
  {-# INLINE beginInt #-}
  {-# INLINE beginList #-}
  {-# INLINE beginDict #-}
  {-# INLINE end #-}
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

class BDecodable a where
  uncons   :: a -> Maybe (Char, a)
  splitAt  :: Integer -> a -> Maybe (a, a)
  null     :: a -> Bool

instance BDecodable [Char] where
  {-# INLINE uncons #-}
  {-# INLINE splitAt #-}
  {-# INLINE null #-}
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
  {-# INLINE uncons #-}
  {-# INLINE splitAt #-}
  {-# INLINE null #-}
  uncons = Text.uncons
  splitAt n s = do
    n' <- integerToInt n
    let (x, xs) = Text.splitAt n' s
    guard (Text.compareLength x n' == EQ)
    return (x, xs)
  null = Text.null

instance BDecodable TextL.Text where
  {-# INLINE uncons #-}
  {-# INLINE splitAt #-}
  {-# INLINE null #-}
  uncons = TextL.uncons
  splitAt n s = do
    n' <- integerToInt64 n
    let (x, xs) = TextL.splitAt n' s
    guard (TextL.compareLength x n' == EQ)
    return (x, xs)
  null = TextL.null

instance BDecodable ByteString where
  {-# INLINE uncons #-}
  {-# INLINE splitAt #-}
  {-# INLINE null #-}
  uncons = C8.uncons
  splitAt n s = do
    n' <- integerToInt n
    let (x, xs) = BS.splitAt n' s
    guard (BS.length x == n')
    return (x, xs)
  null = BS.null

instance BDecodable BSL.ByteString where
  {-# INLINE uncons #-}
  {-# INLINE splitAt #-}
  {-# INLINE null #-}
  uncons = C8L.uncons
  splitAt n s = do
    n' <- integerToInt64 n
    let (x, xs) = BSL.splitAt n' s
    guard (BSL.length x == n')
    return (x, xs)
  null = BSL.null
