module Bitcoin.Keys.WIFSpec (spec) where

import qualified Test.Hspec as H
import qualified Data.ByteString.Char8 as BC

import Bitcoin.Keys.WIF
import Bitcoin.Keys.SEC


spec :: H.Spec
spec = do
    H.describe "WIF format test" $ do
        H.describe "WIF for (Testnet, Compressed)" $ do
            let wif = WIFFormat
                    { network = Testnet
                    , secFormat = Compressed
                    , privateKey = 5003
                    }
                base58Wif = "cMahea7zqjxrtgAbB7LSGbcQUr1uX1ojuat9jZodMN8rFTv2sfUK"
            H.it "Encode WIF" $ do
                let result = encodeWIF wif
                BC.unpack result `H.shouldBe` base58Wif
            H.it "Decode WIF" $ do
                let result = decodeWIF (BC.pack base58Wif)
                result `H.shouldBe` Right wif

        H.describe "WIF for (Testnet, Uncompressed)" $ do
            let wif = WIFFormat
                    { network = Testnet
                    , secFormat = Uncompressed
                    , privateKey = 2021 ^ (5 :: Integer)
                    }
                base58Wif = "91avARGdfge8E4tZfYLoxeJ5sGBdNJQH4kvjpWAxgzczjbCwxic"
            H.it "Encode WIF" $ do
                let result = encodeWIF wif
                BC.unpack result `H.shouldBe` base58Wif
            H.it "Decode WIF" $ do
                let result = decodeWIF (BC.pack base58Wif)
                result `H.shouldBe` Right wif


        H.describe "WIF for (Mainnet, Compressed)" $ do
            let wif = WIFFormat
                    { network = Mainnet
                    , secFormat = Compressed
                    , privateKey = 0x54321deadbeef
                    }
                base58Wif = "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgiuQJv1h8Ytr2S53a"
            H.it "Encode WIF" $ do
                let result = encodeWIF wif
                BC.unpack result `H.shouldBe` base58Wif
            H.it "Decode WIF" $ do
                let result = decodeWIF (BC.pack base58Wif)
                result `H.shouldBe` Right wif

