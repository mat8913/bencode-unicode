module Data.BEncode.Type
( BEncode(..)
) where

data BEncode a = BInt Integer
               | BString a
               | BList [BEncode a]
               | BDict [(a , BEncode a)]
    deriving (Eq, Ord, Read, Show)

instance Functor BEncode where
  fmap _ (BInt x) = BInt x
  fmap f (BString x) = BString (f x)
  fmap f (BList xs) = BList $ map (fmap f) xs
  fmap f (BDict xs) = BDict $ map (\(k, v) -> (f k, fmap f v)) xs
