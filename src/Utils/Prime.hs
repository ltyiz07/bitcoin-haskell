module Utils.Prime
    ( isPrime
    ) where

isPrime :: Integer -> Bool
isPrime n
    | n < 2     = False
    | n == 2    = True
    | even n    = False
    | otherwise = trialDivision n 3
    where
        trialDivision :: Integer -> Integer -> Bool
        trialDivision num divisor
            | divisor * divisor > num = True
            | num `mod` divisor == 0  = False
            | otherwise               = trialDivision num (divisor + 2)


