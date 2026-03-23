{-# LANGUAGE OverloadedStrings #-}

module ECDSA.Utils.HashSpec (spec) where

import qualified Test.Hspec  as H
import qualified Data.Text as T
import ECDSA.Utils.Hash

spec :: H.Spec
spec = do
    H.describe "Hash method test" $ do
        H.it "Hash-256 test" $ do
            (byteStringToHexText . hash256 . textToByteString)  ("This is it!" :: T.Text) `H.shouldBe` ("3d67c04d1cc090e5370ce848ee4907abde1381964cd6fbd2e601a00a0c98656a" :: T.Text)
        H.it "Hash-160 test" $ do
            (byteStringToHexText . hash160 . textToByteString) ("This is it!" :: T.Text) `H.shouldBe` ("ebb2c36181ff5506b46acc67ee64d99202a72159" :: T.Text)
    H.describe "Hmac method test" $ do
        H.it "HMAC SHA-256 test" $ do
            let key = textToByteString ("secretsecret" :: T.Text)
                msg = textToByteString ("This is it!" :: T.Text)
                result = byteStringToHexText (hmac256 key msg)
                expected = "99cbbe09dcfd48dda51bb33e4dc8b794030cbfdc3ed13a4b385ca117a0ccf210" :: T.Text
            result `H.shouldBe` expected
