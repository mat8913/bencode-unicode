module Tests.Gen where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Data.BEncode.Type
import Control.Monad

genBEncode :: Gen a -> Gen (BEncode a)
genBEncode g = sized (\s -> oneof (if s > 1
    then [genBList g, genBDict g]
    else [genBInt, genBString g, genBList g, genBDict g]
    ))

genBInt :: Gen (BEncode a)
genBInt = fmap BInt arbitrary

genBString :: Gen a -> Gen (BEncode a)
genBString = fmap BString

genBList :: Gen a -> Gen (BEncode a)
genBList g = genPartition >>= (fmap BList . mapM (\s -> resize s $ genBEncode g))

genBDict :: Gen a -> Gen (BEncode a)
genBDict g = genPartition >>= (fmap BDict . mapM (\s -> resize s $ genBPair g))

genBPair :: Gen a -> Gen (a , BEncode a)
genBPair g = (,) <$> g <*> genBEncode g

genPartition :: Gen [Int]
genPartition = do
    let helper :: Bool -> [Int] -> [Int]
        helper True acc     = 1 : acc
        helper False (a:cc) = a+1 : cc
    s <- sized return
    case s of
      0 -> return []
      _ -> foldr helper [1] <$> replicateM (s-1) arbitrary
