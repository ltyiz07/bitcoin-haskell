{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Keys.SECSpec (spec) where

import qualified Test.Hspec as H

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16

import ECDSA.Curve.EllipticCurve
import ECDSA.Curve.Secp256k1
import Bitcoin.Keys.SEC


hexToBytes :: B.ByteString -> B.ByteString
hexToBytes bs = case B16.decode bs of
    Left err -> error ("Hex decoding failed: " ++ err)
    Right val -> val

spec :: H.Spec
spec = do
    H.describe "SECFormat test" $ do
        let x :: FG  = 0x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c
            y1 :: FG = 0x61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34
            y2 :: FG = 0x9e21926adce3276fd91d7920c4951b576b5cc871c6c16c5f0ba4999bd65f4dfb

            publicPoint1        = Point x y1
            uncompressedSECKey1 = hexToBytes "04887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34"
            compressedSECKey1   = hexToBytes "02887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c"

            publicPoint2        = Point x y2
            uncompressedSECKey2 = hexToBytes "04887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c9e21926adce3276fd91d7920c4951b576b5cc871c6c16c5f0ba4999bd65f4dfb"
            compressedSECKey2   = hexToBytes "03887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c"

        H.it "Encode SEC Uncompressed format case 1" $ do
            let result :: B.ByteString = encodeSEC Uncompressed publicPoint1
            result `H.shouldBe` uncompressedSECKey1

        H.it "Encode SEC Uncompressed format case 2" $ do
            let result :: B.ByteString = encodeSEC Uncompressed publicPoint2
            result `H.shouldBe` uncompressedSECKey2

        H.it "Encode SEC Compressed format case 1" $ do
            let result :: B.ByteString = encodeSEC Compressed publicPoint1
            result `H.shouldBe` compressedSECKey1

        H.it "Encode SEC Compressed format case 2" $ do
            let result :: B.ByteString = encodeSEC Compressed publicPoint2
            result `H.shouldBe` compressedSECKey2

        H.it "Decode SEC Uncompressed format case 1" $ do
            let result = decodeSEC uncompressedSECKey1
            result `H.shouldBe` Just publicPoint1

        H.it "Decode SEC Uncompressed format case 2" $ do
            let result = decodeSEC uncompressedSECKey2
            result `H.shouldBe` Just publicPoint2

        H.it "Decode SEC Compressed format case 1" $ do
            let result = decodeSEC compressedSECKey1
            result `H.shouldBe` Just publicPoint1

        H.it "Decode SEC Compressed format case 2" $ do
            let result = decodeSEC compressedSECKey2
            result `H.shouldBe` Just publicPoint2
