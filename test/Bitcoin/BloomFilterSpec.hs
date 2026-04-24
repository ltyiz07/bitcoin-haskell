{-# LANGUAGE OverloadedStrings #-}
module Bitcoin.BloomFilterSpec (spec) where

import Test.Hspec
import qualified Data.ByteString as BS
import Bitcoin.BloomFilter
import Data.Word (Word32)

spec :: Spec
spec = do
    describe "MurmurHash3" $ do
        it "empty string with seed 0 should be 0" $
            murmurHash3 0 BS.empty `shouldBe` 0
        
        it "match known value for 'hello world' with seed 0" $
            -- MurmurHash3 x86 32-bit of "hello world" with seed 0 is 0x5ee28924
            murmurHash3 0 "hello world" `shouldBe` 0x5e928f0f

    describe "BloomFilter" $ do
        let bf = newBloomFilter 10 3 0x12345678 1
        
        it "newly created filter should not contain anything" $
            bloomFilterContains bf "hello" `shouldBe` False
        
        it "should contain added item" $
            let bf' = bloomFilterAdd bf "hello"
            in bloomFilterContains bf' "hello" `shouldBe` True
            
        it "should contain multiple added items" $
            let bf' = bloomFilterAdd (bloomFilterAdd bf "hello") "world"
            in do
                bloomFilterContains bf' "hello" `shouldBe` True
                bloomFilterContains bf' "world" `shouldBe` True
                bloomFilterContains bf' "bitcoin" `shouldBe` False
