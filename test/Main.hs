module Main (main) where

import Test.Hspec
import qualified Utils.PrimeSpec
import qualified Utils.HashSpec
import qualified Utils.DetermineKSpec
import qualified FiniteFieldSpec
import qualified RationalFieldSpec
import qualified EllipticCurveSpec
import qualified Secp256k1Spec

main :: IO ()
main = hspec $ do
    Utils.PrimeSpec.spec
    Utils.HashSpec.spec
    Utils.DetermineKSpec.spec
    FiniteFieldSpec.spec
    RationalFieldSpec.spec
    EllipticCurveSpec.spec
    Secp256k1Spec.spec
