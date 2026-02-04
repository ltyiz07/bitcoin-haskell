module EllipticCurveSpec (spec) where

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import Field
import Field.FiniteField
import Field.RealField
import EllipticCurve

spec :: H.Spec
spec = do
    H.describe "method: isOnCurve for RealField" $ do
        H.it "point at infinity should be on curve" $ do
            let curve = mkEllipticCurve (mkRealField 5) (mkRealField 7)
            isOnCurve curve Infinity `H.shouldBe` True
        H.it "point is on or off curve" $ do
            let curve = mkEllipticCurve (mkRealField 5) (mkRealField 7)
                p0 = mkPoint (mkRealField (-1)) (mkRealField 1)
                p1 = mkPoint (mkRealField 2) (mkRealField 4)
                p2 = mkPoint (mkRealField (-1)) (mkRealField (-1))
                p3 = mkPoint (mkRealField 18) (mkRealField 77)
                p4 = mkPoint (mkRealField 5) (mkRealField 7)
            isOnCurve curve p0 `H.shouldBe` True
            isOnCurve curve p1 `H.shouldBe` False
            isOnCurve curve p2 `H.shouldBe` True
            isOnCurve curve p3 `H.shouldBe` True
            isOnCurve curve p4 `H.shouldBe` False
        H.it "make point on curve" $ do
            let curve = mkEllipticCurve (mkRealField 5) (mkRealField 7)
            mkPointOnCurve curve (mkRealField (-1)) (mkRealField 1) `H.shouldBe` Just (Point (mkRealField (-1)) (mkRealField 1))
            mkPointOnCurve curve (mkRealField 2) (mkRealField 4) `H.shouldBe` Nothing

    H.describe "method: isOnCurve for FiniteField" $ do
        H.it "point is on curve" $ do
            let mkff = flip mkFiniteField 103
                curve = mkEllipticCurve a b
                    where
                        Just a = mkff 0
                        Just b = mkff 7
                p = mkPoint x y
                    where Just x = mkff 17; Just y = mkff 64
            isOnCurve curve p `H.shouldBe` True
        H.it "point is on or off curve" $ do
            let mkff = flip mkFiniteField 223
                curve = mkEllipticCurve a b
                    where
                        Just a = mkff 0
                        Just b = mkff 7
                p1 = mkPoint x y
                    where Just x = mkff 192; Just y = mkff 105
                p2 = mkPoint x y
                    where Just x = mkff 17; Just y = mkff 56
                p3 = mkPoint x y
                    where Just x = mkff 200; Just y = mkff 119
                p4 = mkPoint x y
                    where Just x = mkff 1; Just y = mkff 193
                p5 = mkPoint x y
                    where Just x = mkff 42; Just y = mkff 99
            isOnCurve curve p1 `H.shouldBe` True
            isOnCurve curve p2 `H.shouldBe` True
            isOnCurve curve p3 `H.shouldBe` False
            isOnCurve curve p4 `H.shouldBe` True
            isOnCurve curve p5 `H.shouldBe` False

    H.describe "method: addPoint for RealField" $ do
        H.it "add with point at infinity" $ do
            let curve = mkEllipticCurve (mkRealField 5) (mkRealField 7)
                Just p1 = mkPointOnCurve curve (mkRealField (-1)) (mkRealField 1)
            addPoint curve Infinity Infinity `H.shouldBe` Infinity
            addPoint curve Infinity p1 `H.shouldBe` p1
            addPoint curve p1 Infinity `H.shouldBe` p1

    H.describe "--WIP case start" $ do
        H.it "WIP case end--" $ do
            print $ v
                where
                    v = "working on"

