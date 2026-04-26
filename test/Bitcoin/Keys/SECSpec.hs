{-# LANGUAGE OverloadedRecordDot #-}

module Bitcoin.Keys.SECSpec (spec) where

import qualified Test.Hspec as H

import Utils.Hex
import Bitcoin.Keys.SEC


spec :: H.Spec
spec = do
    H.describe "SECFormat test" $ do
        let x  = 0x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c
            y1 = 0x61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34
            y2 = 0x9e21926adce3276fd91d7920c4951b576b5cc871c6c16c5f0ba4999bd65f4dfb

            uncompressedSECKey1 = hexToBytes "04887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34"
            compressedSECKey1   = hexToBytes "02887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c"

            uncompressedSECKey2 = hexToBytes "04887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c9e21926adce3276fd91d7920c4951b576b5cc871c6c16c5f0ba4999bd65f4dfb"
            compressedSECKey2   = hexToBytes "03887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c"

        H.it "Encode SEC Uncompressed format case 1" $ do
            let result = Uncompressed x y1
            putSEC result `H.shouldBe` uncompressedSECKey1

        H.it "Encode SEC Uncompressed format case 2" $ do
            let result = Uncompressed x y2
            putSEC result `H.shouldBe` uncompressedSECKey2

        H.it "Encode SEC Compressed format case 1" $ do
            let result = Compressed x True
            putSEC result `H.shouldBe` compressedSECKey1

        H.it "Encode SEC Compressed format case 2" $ do
            let result = Compressed x False
            putSEC result `H.shouldBe` compressedSECKey2

        H.it "Decode SEC Uncompressed format case 1" $ do
            let result = getSEC uncompressedSECKey1
            result `H.shouldBe` Right (Uncompressed x y1)

        H.it "Decode SEC Uncompressed format case 2" $ do
            let result = getSEC uncompressedSECKey2
            result `H.shouldBe` Right (Uncompressed x y2)

        H.it "Decode SEC Compressed format case 1" $ do
            let result = getSEC compressedSECKey1
            result `H.shouldBe` Right (Compressed x True)

        H.it "Decode SEC Compressed format case 2" $ do
            let result = getSEC compressedSECKey2
            result `H.shouldBe` Right (Compressed x False)
