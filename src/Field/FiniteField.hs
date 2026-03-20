{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Field.FiniteField
    ( FiniteField(..)
    ) where

import GHC.TypeLits
import Data.Proxy
import Data.Ratio -- For fromRational

newtype FiniteField (p :: Nat) = FiniteField Integer
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
           else FiniteField (modInverse a p_val)

    fromRational r =
        let p_val = natVal (Proxy @p)
            n = numerator r
            d = denominator r
            d_inv = modInverse d p_val
        in FiniteField ((n * d_inv) `mod` p_val)

-- | 내부 헬퍼 함수: 모듈러 역원
modInverse :: Integer -> Integer -> Integer
modInverse a m = let (_, x, _) = extendedGCD a m
                 in x `mod` m

-- | 내부 헬퍼 함수: 확장 유클리드 호제법
extendedGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extendedGCD a 0 = (a, 1, 0)
extendedGCD a b = let (g, x1, y1) = extendedGCD b (a `mod` b)
                      x = y1
                      y = x1 - (a `Prelude.div` b) * y1
                  in (g, x, y)