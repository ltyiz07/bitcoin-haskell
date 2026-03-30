{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Keys.DERSpec (spec) where

import qualified Test.Hspec as H

import Bitcoin.Keys.DER
import Bitcoin.Utils.Hex

spec :: H.Spec
spec = do
    H.describe "DER format test" $ do
        let r = 0x1f62993ee03fca342fcb45929993fa6ee885e00ddad8de154f268d98f0839914
            s = 0x1e1ca12ad140c04e0e022c38f7ce31da426b8009d02832f0b44f39a6b178b7a1
            der = hexToBytes "304402201f62993ee03fca342fcb45929993fa6ee885e00ddad8de154f268d98f083991402201e1ca12ad140c04e0e022c38f7ce31da426b8009d02832f0b44f39a6b178b7a1"
        H.it "Encode DER" $ do
            encodeDER (r, s) `H.shouldBe` der
        H.it "Decode DER" $ do
            decodeDER der `H.shouldBe` Right (r, s)

