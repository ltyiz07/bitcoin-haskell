module Main (main) where

import Test.Hspec
import qualified Utils.PrimeSpec
import qualified Utils.HashSpec
import qualified Utils.DetermineKSpec
import qualified FiniteFieldSpec
import qualified RationalFieldSpec
import qualified EllipticCurveSpec
import qualified SignatureSpec

main :: IO ()
main = hspec $ do
    Utils.PrimeSpec.spec
    Utils.HashSpec.spec
    Utils.DetermineKSpec.spec
    FiniteFieldSpec.spec
    RationalFieldSpec.spec
    EllipticCurveSpec.spec
    SignatureSpec.spec
