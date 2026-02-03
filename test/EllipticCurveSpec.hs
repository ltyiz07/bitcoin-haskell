module EllipticCurveSpec (spec) where

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import Field
import Field.FiniteField
import Field.RealField
import EllipticCurve

spec :: H.Spec
spec = do
    H.describe "method: mkPoint" $ do
        H.it "Point with valid value" $ do
            let ff11 = flip mkFiniteField 11
                p1 = mkPoint <$> ff11 2 <*> ff11 3
                p2 = mkPoint <$> ff11 2 <*> ff11 3
            p1 `H.shouldBe` p2
    H.describe "method: isOnCurve" $ do
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

    H.describe "--WIP case start" $ do
        H.it "WIP case end--" $ do
            print $ v
                where
                    v = "hello"

