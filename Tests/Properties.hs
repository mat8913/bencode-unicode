module Main (main) where

import System.Exit
import Tests.Gen

import Test.QuickCheck
import Test.QuickCheck.Test
import Test.QuickCheck.Monadic
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import qualified Test.QuickCheck.Unicode as Unicode

import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy.BS

import Data.BEncode
import Data.BEncode.Class

genBEncoded :: BEncodable a b => Gen a -> Gen a
genBEncoded = fmap bencode . genBEncode

genLazyText :: Gen Lazy.Text
genLazyText = Lazy.Text.pack <$> Unicode.string

genLazyByteString :: Gen Lazy.ByteString
genLazyByteString = Lazy.BS.pack <$> arbitrary

main :: IO ()
main = runTests [ testProperty "prop_redecode (String)"          (prop_redecode Unicode.string)
                , testProperty "prop_redecode (Lazy.Text)"       (prop_redecode genLazyText)
                , testProperty "prop_redecode (Lazy.ByteString)" (prop_redecode genLazyByteString)
                , testProperty "Text === Lazy.Text"              (prop_sameDecode genLazyText Lazy.Text.toStrict)
                , testProperty "ByteString === Lazy.ByteString"  (prop_sameDecode genLazyByteString Lazy.BS.toStrict)
                ]

prop_redecode :: (BDecodable a, BEncodable a b, Eq a, Show a) => Gen a -> Property
prop_redecode g = forAll (genBEncode g) $ \d -> Just d == bdecode (bencode d)

prop_sameDecode :: (Eq s, BDecodable s, BDecodable l, BEncodable l b, Show l)
                => Gen l -> (l -> s) -> Property
prop_sameDecode g f = forAll (genBEncoded g) $ \e -> let Just d = bdecode e in
    bdecode (f e) == Just (fmap f d)

runTests :: [(String, IO Result)] -> IO ()
runTests [] = return ()
runTests ((name, prop):xs) = do
    putStrLn ("Testing " ++ name)
    result <- prop
    if isSuccess result then runTests xs
                        else exitFailure

testProperty :: Testable prop => String -> prop -> (String, IO Result)
testProperty name prop = (name, quickCheckResult prop)
