module EllipticCurveSpec (spec) where

import qualified Test.Hspec as H
import Data.Maybe (fromJust)

import ECDSA.Field.FiniteField
import ECDSA.Field.RationalField
import ECDSA.Curve.EllipticCurve

-- For cleaner test code with type-safe FiniteFields
type F223 = FiniteField 223
type F103 = FiniteField 103

type FG = FiniteField 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F

spec :: H.Spec
spec = do
    H.describe "method: isOnCurve for RationalField" $ do
        H.it "point at infinity should be on curve" $ do
            let curve = mkEllipticCurve 5 7 :: EllipticCurve RationalField
            isOnCurve curve Infinity `H.shouldBe` True
        H.it "point is on or off curve" $ do
            let curve = mkEllipticCurve 5 7 :: EllipticCurve RationalField
                p0 = Point (-1) 1
                p1 = Point 2 4
                p2 = Point (-1) (-1)
                p3 = Point 18 77
                p4 = Point 5 7
            isOnCurve curve p0 `H.shouldBe` True
            isOnCurve curve p1 `H.shouldBe` False
            isOnCurve curve p2 `H.shouldBe` True
            isOnCurve curve p3 `H.shouldBe` True
            isOnCurve curve p4 `H.shouldBe` False
        H.it "make point on curve" $ do
            let curve = mkEllipticCurve 5 7 :: EllipticCurve RationalField
            mkPointOnCurve curve (-1) 1 `H.shouldBe` Just (Point (-1) 1)
            mkPointOnCurve curve 2 4 `H.shouldBe` Nothing

    H.describe "method: isOnCurve for FiniteField" $ do
        H.it "point is on curve" $ do
            let curve = mkEllipticCurve 0 7 :: EllipticCurve F103
                p = Point 17 64
            isOnCurve curve p `H.shouldBe` True
        H.it "point is on or off curve" $ do
            let curve = mkEllipticCurve 0 7 :: EllipticCurve F223
                p1 = Point 192 105
                p2 = Point 17 56
                p3 = Point 200 119
                p4 = Point 1 193
                p5 = Point 42 99
            isOnCurve curve p1 `H.shouldBe` True
            isOnCurve curve p2 `H.shouldBe` True
            isOnCurve curve p3 `H.shouldBe` False
            isOnCurve curve p4 `H.shouldBe` True
            isOnCurve curve p5 `H.shouldBe` False

    H.describe "method: addPoint for RationalField" $ do
        H.it "add with point at infinity" $ do
            let curve = mkEllipticCurve 5 7 :: EllipticCurve RationalField
                p1 = fromJust $ mkPointOnCurve curve (-1) 1
            addPoint curve Infinity Infinity `H.shouldBe` Infinity
            addPoint curve Infinity p1 `H.shouldBe` p1
            addPoint curve p1 Infinity `H.shouldBe` p1
        H.it "add same points with perpendicular to the x-axis and tangent to curve (y=0)" $ do
            let curve1 = mkEllipticCurve 6 7 :: EllipticCurve RationalField
                p1 = fromJust $ mkPointOnCurve curve1 (-1) 0
            let curve2 = mkEllipticCurve 3 4 :: EllipticCurve RationalField
                p2 = fromJust $ mkPointOnCurve curve2 (-1) 0
            addPoint curve1 p1 p1 `H.shouldBe` Infinity
            addPoint curve2 p2 p2 `H.shouldBe` Infinity
        H.it "add same points" $ do
            let curve = mkEllipticCurve 5 7 :: EllipticCurve RationalField
                p = fromJust $ mkPointOnCurve curve (-1) (-1)
            addPoint curve p p `H.shouldBe` Point 18 77
        H.it "add different points" $ do
            let curve = mkEllipticCurve 5 7 :: EllipticCurve RationalField
                p1 = fromJust $ mkPointOnCurve curve 2 5
                p2 = fromJust $ mkPointOnCurve curve (-1) (-1)
            addPoint curve p1 p2 `H.shouldBe` Point 3 (-7)

    H.describe "method addPoint for FiniteField" $ do
        H.it "add points (192,105) + (17,56)" $ do
            let curve = mkEllipticCurve 0 7 :: EllipticCurve F223
                p1 = fromJust $ mkPointOnCurve curve 192 105
                p2 = fromJust $ mkPointOnCurve curve 17 56
                p3 = Point 170 142
            addPoint curve p1 p2 `H.shouldBe` p3
        H.it "add points (47,71) + (117,141)" $ do
            let curve = mkEllipticCurve 0 7 :: EllipticCurve F223
                p1 = fromJust $ mkPointOnCurve curve 47 71
                p2 = fromJust $ mkPointOnCurve curve 117 141
                p3 = Point 60 139
            addPoint curve p1 p2 `H.shouldBe` p3
        H.it "add points (143,98) + (76,66)" $ do
            let curve = mkEllipticCurve 0 7 :: EllipticCurve F223
                p1 = fromJust $ mkPointOnCurve curve 143 98
                p2 = fromJust $ mkPointOnCurve curve 76 66
                p3 = Point 47 71
            addPoint curve p1 p2 `H.shouldBe` p3
        H.it "add same points (47,71) + (47,71)" $ do
            let curve = mkEllipticCurve 0 7 :: EllipticCurve F223
                p1 = fromJust $ mkPointOnCurve curve 47 71
            addPoint curve p1 p1 `H.shouldBe` Point 36 111

    H.describe "method multiplyPoint for FiniteField" $ do
        H.it "compare result with addPoint method" $ do
            let curve = mkEllipticCurve 0 7 :: EllipticCurve FG
                p1 = fromJust $ mkPointOnCurve curve 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
                addedPoint2 = addPoint curve p1 p1
                addedPoint3 = addPoint curve addedPoint2 p1
                addedPoint4 = addPoint curve addedPoint3 p1
                addedPoint8 = addPoint curve addedPoint4 addedPoint4
            multiplyPoint curve 2 p1 `H.shouldBe` addedPoint2
            multiplyPoint curve 3 p1 `H.shouldBe` addedPoint3
            multiplyPoint curve 4 p1 `H.shouldBe` addedPoint4
            multiplyPoint curve 8 p1 `H.shouldBe` addedPoint8

    H.describe "--WIP case start" $ do
        H.it "WIP case end--" $ do
            let target = "hello"
            print $ target
