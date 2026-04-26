{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Keys.WIFSpec (spec) where

import qualified Test.Hspec as H
import qualified Data.ByteString.Char8 as BC

import Bitcoin.Keys.WIF
import Bitcoin.Keys.SEC


spec :: H.Spec
spec = do
    H.describe "WIF format test" $ do
        H.describe "WIF for (Testnet, Compressed)" $ do
            let wif = WIF
                    { wifPrefix = 0xef
                    , wifSecret = 5003
                    , wifCompressed = True
                    }
                base58Wif = "cMahea7zqjxrtgAbB7LSGbcQUr1uX1ojuat9jZodMN8rFTv2sfUK"
            H.it "Encode WIF" $ do
                let result = toWIFText wif
                result `H.shouldBe` base58Wif
            H.it "Decode WIF" $ do
                let result = fromWIFText base58Wif
                result `H.shouldBe` Right wif

        H.describe "WIF for (Testnet, Uncompressed)" $ do
            let wif = WIF
                    { wifPrefix = 0xef
                    , wifSecret = 2021 ^ (5 :: Integer)
                    , wifCompressed = False
                    }
                base58Wif = "91avARGdfge8E4tZfYLoxeJ5sGBdNJQH4kvjpWAxgzczjbCwxic"
            H.it "Encode WIF" $ do
                let result = toWIFText wif
                result `H.shouldBe` base58Wif
            H.it "Decode WIF" $ do
                let result = fromWIFText base58Wif
                result `H.shouldBe` Right wif


        H.describe "WIF for (Mainnet, Compressed)" $ do
            let wif = WIF
                    { wifPrefix = 0x80
                    , wifSecret = 0x54321deadbeef
                    , wifCompressed = True
                    }
                base58Wif = "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgiuQJv1h8Ytr2S53a"
            H.it "Encode WIF" $ do
                let result = toWIFText wif
                result `H.shouldBe` base58Wif
            H.it "Decode WIF" $ do
                let result = fromWIFText base58Wif
                result `H.shouldBe` Right wif

