module EllipticCurve
    ( Point(..)
    , EllipticCurve(..)
    , mkPoint
    , mkEllipticCurve
    , isOnCurve
    ) where

import Field


data Point f
    = Point
        { x :: f
        , y :: f
        }
    | Infinity
    deriving (Show, Eq)

-- y^2 = x^3 + a*x + b
data EllipticCurve f = EllipticCurve
    { a :: f
    , b :: f
    } deriving (Show, Eq)

mkEllipticCurve :: (Field f, Eq f) => f -> f -> EllipticCurve f
mkEllipticCurve a b = EllipticCurve a b

mkPoint :: (Field f, Eq f) => f -> f -> Point f
mkPoint x y = Point x y

isOnCurve :: (Field f, Eq f) => EllipticCurve f -> Point f -> Bool
-- isOnCurve _ Infinity = True
isOnCurve (EllipticCurve a b) (Point x y)
    | lhs == rhs = True
    | otherwise = False
        where
            lhs = Just (pow y 2)
            rhs = do
                v1 <- Just (pow x 3)
                v2 <- mult a x
                v3 <- add v1 v2
                add v3 b

