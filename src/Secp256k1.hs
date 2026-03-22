{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Secp256k1
    ( FG
    , secp256k1
    , secp256k1Order
    , gPoint
    ) where

import EllipticCurve
import Field.FiniteField
import Utils.Arithmetic

-- Prime order: (2^256 - 2^32 - 2^9 - 2^8 - 2^7 - 2^6 - 2^4 - 1)
type FG = FiniteField 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F

secp256k1 :: EllipticCurve FG
secp256k1 = EllipticCurve
    { a = 0
    , b = 7
    }

secp256k1Order :: Integer
secp256k1Order = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141

gPoint :: Point FG
gPoint = Point
    { x = 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
    , y = 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
    }

