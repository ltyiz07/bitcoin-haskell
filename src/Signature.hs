module Signature
    ( Signature(..)
    , verify
    ) where

import Secp256k1
import EllipticCurve
import Utils.Arithmetic
import Field.FiniteField

data Signature = Signature
    { r :: Integer
    , s :: Integer
    } deriving (Show, Eq)

{-
 - # to verivy
 -   - z: Message hash (of Hash-256)
 -   - publicPoint: Public key
 -   - r: x value of Random point (R === kG)
 -   - s: Signature
 -}

verify :: Integer -> Point FG -> Signature -> Bool
verify z publicPoint (Signature r s)
    | r <= 0 || r >= secp256k1Order = False
    | s <= 0 || s >= secp256k1Order = False
    | otherwise = 
        let invS = invMod s secp256k1Order
            u = (z * invS) `mod` secp256k1Order
            v = (r * invS) `mod` secp256k1Order
            uG = multiplyPoint secp256k1 u gPoint
            vP = multiplyPoint secp256k1 v publicPoint
            addedPoint = addPoint secp256k1 uG vP
        in case addedPoint of
            Infinity -> False
            Point rx _ -> getValue rx == r

