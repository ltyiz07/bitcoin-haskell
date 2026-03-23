{-# LANGUAGE OverloadedStrings #-}

module SignatureSpec (spec) where

import qualified Test.Hspec as H
import ECDSA.Curve.EllipticCurve
import ECDSA.Signature
import ECDSA.Curve.Secp256k1
import ECDSA.Utils.DetermineK
import ECDSA.Utils.Arithmetic
import ECDSA.Field.FiniteField
-- import qualified Data.Text as T
-- import Utils.Hash

spec :: H.Spec
spec = do
    H.describe "ECDSA verify method test" $ do
        let px :: FG = 0x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c
            py :: FG = 0x61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34
        H.it "test case 1" $ do
            let z = 0xec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60
                r = 0xac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395
                s = 0x68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4
            verify (Point px py) (Signature r s) z `H.shouldBe` True
        H.it "test case 2" $ do
            let z = 0x7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d
                r = 0xeff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c
                s = 0xc7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab6
            verify (Point px py) (Signature r s) z `H.shouldBe` True

    H.describe "ECDSA sign method test" $ do
        let e           = 0x012345
            publicPoint = derivePublicPoint e
            z           = 0x3d67c04d1cc090e5370ce848ee4907abde1381964cd6fbd2e601a00a0c98656a
        H.it "test case 1" $ do
            let k = determineK e z
                r = getValue $ x (multiplyPoint secp256k1 k gPoint) -- mod n 추가
                s = ((z + r * e) * (invMod k secp256k1Order)) `mod` secp256k1Order -- Low-S 사용하도록 수정
            verify publicPoint (Signature r s) z `H.shouldBe` True
            -- https://datatracker.ietf.org/doc/html/rfc6979#section-3.2
        H.it "test sign method" $ do
            let sig = sign e z
            verify publicPoint sig z `H.shouldBe` True

