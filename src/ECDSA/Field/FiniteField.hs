module ECDSA.Field.FiniteField
    ( FiniteField(..)
    , sqrtModP 
    ) where

import GHC.TypeLits
import Data.Proxy
import Data.Ratio -- For fromRational

import Utils.Arithmetic


newtype FiniteField (p :: Nat) = FiniteField { getValue :: Integer }
    deriving (Eq, Show)

instance KnownNat p => Num (FiniteField p) where
    (FiniteField a) + (FiniteField b) =
        let p_val = natVal (Proxy @p)
        in FiniteField ((a + b) `mod` p_val)

    (FiniteField a) - (FiniteField b) =
        let p_val = natVal (Proxy @p)
        in FiniteField ((a - b) `mod` p_val)

    (FiniteField a) * (FiniteField b) =
        let p_val = natVal (Proxy @p)
        in FiniteField ((a * b) `mod` p_val)

    negate (FiniteField a) =
        let p_val = natVal (Proxy @p)
        in FiniteField (negate a `mod` p_val)

    abs = id -- In a finite field, abs typically doesn't apply in the usual sense. Identity is a safe default.
    signum (FiniteField a) =
        if a == 0 then FiniteField 0
        else FiniteField 1 -- In a field, everything is either 0 or has a multiplicative inverse.

    fromInteger n =
        let p_val = natVal (Proxy @p)
        in FiniteField (n `mod` p_val)

instance KnownNat p => Fractional (FiniteField p) where
    recip (FiniteField a) =
        let p_val = natVal (Proxy @p)
        in if a == 0
           then error "FiniteField: division by zero (reciprocal of 0)"
           else FiniteField (invMod a p_val)

    fromRational r =
        let p_val = natVal (Proxy @p)
            n = numerator r
            d = denominator r
            d_inv = invMod d p_val
        in FiniteField ((n * d_inv) `mod` p_val)

-- Return: (Even-Y, Odd-Y)
sqrtModP :: forall p. KnownNat p => FiniteField p -> Maybe (FiniteField p, FiniteField p)
sqrtModP n =
    let p_val     = natVal (Proxy @p)
        expo  = (p_val + 1) `quot` 4
        candidate = fromInteger $ powMod (getValue n) expo p_val
    in if p_val `mod` 4 /= 3
       then Nothing
       else if candidate * candidate == n
            then
                let evenY = if even (getValue candidate) then candidate else negate candidate
                    oddY  = negate evenY
                in Just (evenY, oddY)
            else Nothing
