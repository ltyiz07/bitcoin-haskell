module Main (main) where

import Test.Hspec
import qualified PrimeSpec
import qualified FiniteFieldSpec
import qualified RealFieldSpec
import qualified EllipticCurveSpec

main :: IO ()
main = hspec $ do
    PrimeSpec.spec
    FiniteFieldSpec.spec
    RealFieldSpec.spec
    EllipticCurveSpec.spec
