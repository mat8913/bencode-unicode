{-# LANGUAGE MagicHash #-}

module Data.BEncode.IntConv
( integerToInt
, integerToInt64
) where

import GHC.Integer.GMP.Internals (Integer(S#))
import GHC.Types (Int(I#))
import GHC.Int (Int64(I64#))

integerToInt :: Integer -> Maybe Int
integerToInt (S# i) = Just (I# i)
integerToInt _      = Nothing

integerToInt64 :: Integer -> Maybe Int64
integerToInt64 (S# i) = Just (I64# i)
integerToInt64 _      = Nothing

{-# INLINE integerToInt #-}
{-# INLINE integerToInt64 #-}
