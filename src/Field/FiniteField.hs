module Field.FiniteField
    ( FiniteField(..)
    , mkFiniteField
    ) where

import Field

data FiniteField = FiniteField
    { value :: Integer
    , prime :: Integer
    } deriving (Show, Eq)

instance Field FiniteField where
    add (FiniteField a p1) (FiniteField b p2)
        | p1 /= p2 = Nothing
        | otherwise = Just $ FiniteField ((a + b) `mod` p1) p1

    sub (FiniteField a p1) (FiniteField b p2)
        | p1 /= p2 = Nothing
        | otherwise = Just $ FiniteField ((a - b) `mod` p1) p1

    mult (FiniteField a p1) (FiniteField b p2)
        | p1 /= p2 = Nothing
        | otherwise = Just $ FiniteField ((a * b) `mod` p1) p1

    divide ff1 ff2 = do
        invVal <- inv ff2
        mult ff1 invVal

    pow (FiniteField a p) n = FiniteField (modPow a n p) p
        where
            modPow :: Integer -> Integer -> Integer -> Integer
            modPow base exp modulus
                | exp < 0 = modPow (modInverse base modulus) (-exp) modulus
                | exp == 0 = 1
                | even exp = modPow ((base * base) `mod` modulus) (exp `div` 2) modulus
                | otherwise = (base * modPow base (exp - 1) modulus) `mod` modulus
    inv (FiniteField a p)
        | a == 0 = Nothing
        | otherwise = Just $ FiniteField (modInverse a p) p

mkFiniteField :: Integer -> Integer -> Maybe FiniteField
mkFiniteField v p
    | v < 0 || v >= p = Nothing
    | otherwise       = Just $ FiniteField v p

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
