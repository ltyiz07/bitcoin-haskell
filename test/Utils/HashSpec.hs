{-# LANGUAGE OverloadedStrings #-}

module Utils.HashSpec (spec) where

import qualified Test.Hspec  as H
import qualified Data.Text as T
import Utils.Hash

spec :: H.Spec
spec = do
    H.describe "Hash method test" $ do
        H.it "Hash-256 test" $ do
            (byteStringToHexText . hash256 . textToByteString)  ("This is it!" :: T.Text) `H.shouldBe` ("3d67c04d1cc090e5370ce848ee4907abde1381964cd6fbd2e601a00a0c98656a" :: T.Text)
        H.it "Hash-160 test" $ do
            (byteStringToHexText . hash160 . textToByteString) ("This is it!" :: T.Text) `H.shouldBe` ("ebb2c36181ff5506b46acc67ee64d99202a72159" :: T.Text)
