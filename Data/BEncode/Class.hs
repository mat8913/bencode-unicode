{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

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
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Data.Text.Lazy.Builder.Int as TextBuilder

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.ByteString.Builder as BSBuilder

import Data.IntCast


class BEncodable a b | a -> b, b -> a where
  collect     :: [b] -> b
  inject      :: a -> b
  injectLen   :: a -> b
  injectInt   :: Integer -> b
  build       :: b -> a
  beginString :: b
  beginInt    :: b
  beginList   :: b
  beginDict   :: b
  end         :: b

instance BEncodable [Char] ShowS where
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
  collect = foldr (.) id
  inject = showString
  injectLen = shows . length
  injectInt = shows
  build x = x ""
  beginString = showString ":"
  beginInt    = showString "i"
  beginList   = showString "l"
  beginDict   = showString "d"
  end         = showString "e"

instance BEncodable TextL.Text TextBuilder.Builder where
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
  collect = mconcat
  inject = TextBuilder.fromLazyText
  injectLen = TextBuilder.decimal . TextL.length
  injectInt = TextBuilder.decimal
  build = TextBuilder.toLazyText
  beginString = TextBuilder.singleton ':'
  beginInt    = TextBuilder.singleton 'i'
  beginList   = TextBuilder.singleton 'l'
  beginDict   = TextBuilder.singleton 'd'
  end         = TextBuilder.singleton 'e'

instance BEncodable BSL.ByteString BSBuilder.Builder where
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
  collect = mconcat
  inject = BSBuilder.lazyByteString
  injectLen = BSBuilder.int64Dec . BSL.length
  injectInt = BSBuilder.integerDec
  build = BSBuilder.toLazyByteString
  beginString = BSBuilder.char7 ':'
  beginInt    = BSBuilder.char7 'i'
  beginList   = BSBuilder.char7 'l'
  beginDict   = BSBuilder.char7 'd'
  end         = BSBuilder.char7 'e'

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
    n' <- intCastMaybe n
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
    n' <- intCastMaybe n
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
    n' <- intCastMaybe n
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
    n' <- intCastMaybe n
    let (x, xs) = BSL.splitAt n' s
    guard (BSL.length x == n')
    return (x, xs)
  null = BSL.null
