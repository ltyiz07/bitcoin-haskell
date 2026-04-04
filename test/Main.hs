module Main (main) where

import Test.Hspec

import qualified Utils.PrimeSpec
import qualified Utils.HashSpec
import qualified ECDSA.Field.FiniteFieldSpec
import qualified ECDSA.Field.RationalFieldSpec
import qualified ECDSA.Curve.EllipticCurveSpec
import qualified ECDSA.SignatureSpec
import qualified Bitcoin.Keys.SECSpec
import qualified Bitcoin.Keys.DERSpec
import qualified Bitcoin.Keys.WIFSpec
import qualified Bitcoin.TransactionSpec
import qualified Bitcoin.Network.DiscoverySpec


main :: IO ()
main = hspec $ do
    Utils.PrimeSpec.spec
    Utils.HashSpec.spec
    ECDSA.Field.FiniteFieldSpec.spec
    ECDSA.Field.RationalFieldSpec.spec
    ECDSA.Curve.EllipticCurveSpec.spec
    ECDSA.SignatureSpec.spec
    Bitcoin.Keys.SECSpec.spec
    Bitcoin.Keys.DERSpec.spec
    Bitcoin.Keys.WIFSpec.spec
    Bitcoin.TransactionSpec.spec
    Bitcoin.Network.DiscoverySpec.spec
