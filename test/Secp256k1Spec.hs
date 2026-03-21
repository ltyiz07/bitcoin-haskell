module Secp256k1Spec (spec) where

import qualified Test.Hspec as H
import Text.Printf (printf)
import Field.FiniteField
import EllipticCurve
import Secp256k1
import Utils.Arithmetic

spec :: H.Spec
spec = do
    H.describe "EllipticCurve instance Secp256k1--WIP case start" $ do
        H.it "WIP case end--" $ do
            let px :: FG = 0x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c
                py :: FG = 0x61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34
                z = 0xec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60
                r = 0xac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395
                s = 0x68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4
            let u = z * (powMod s (n - 2) n) `mod` n
                v = r * (powMod s (n - 2) n) `mod` n
                uG = multiplyPoint secp256k1 u gPoint
                vP = multiplyPoint secp256k1 v (Point px py)
                Point rx ry = addPoint secp256k1 uG vP
            getValue rx `H.shouldBe` r
            printf "x-coordinate: %064x\n" (getValue rx)
            printf "y-coordinate: %064x\n" (getValue ry)
            let ySquare :: FG = fromInteger (r ^ 3 + 7)
                yVal = powMod (getValue ySquare) ((p + 1) `div` 4) p
                y :: FG = fromInteger yVal
            printf "y-coordinate: %064x\n" (getValue (negate y))
            print $ isOnCurve secp256k1 (Point rx ry)
            print $ isOnCurve secp256k1 (Point rx y)
            print $ isOnCurve secp256k1 (Point rx (negate y))


