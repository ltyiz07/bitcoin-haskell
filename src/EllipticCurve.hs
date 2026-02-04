module EllipticCurve
    ( Point(..)
    , EllipticCurve(..)
    , mkPoint
    , mkEllipticCurve
    , mkPointOnCurve
    , isOnCurve
    , addPoint
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

mkPointOnCurve :: (Field f, Eq f) => EllipticCurve f -> f -> f -> Maybe (Point f)
mkPointOnCurve curve x y
    | isOnCurve curve point = Just point
    | otherwise             = Nothing
        where point = mkPoint x y

isOnCurve :: (Field f, Eq f) => EllipticCurve f -> Point f -> Bool
isOnCurve _ Infinity = True
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

addPoint :: (Field f, Eq f) => EllipticCurve f -> Point f -> Point f -> Point f
addPoint _ Infinity Infinity = Infinity
addPoint _ Infinity point2 = point2
addPoint _ point1 Infinity = point1
addPoint (EllipticCurve a b) (Point x1 y1) (Point x2 y2)
    | x1 == x2 && y1 == y2 && y1 == fromIntWith y1 0 = Infinity
    | x1 == x2 && y1 == y2 = Point x3 y3
        where
            maybeS = do
                tl <- mult (fromIntWith x1 3) (pow x1 2)
                t <- add tl a
                b <- mult (fromIntWith y1 2) y1
                divide t b
            maybeX3 = do
                s <- maybeS
                sSquare <- Just (pow s 2)
                twoX1 <- mult (fromIntWith x1 2) x1
                sub sSquare twoX1
            maybeY3 = do
                x3 <- maybeX3
                x1SubX3 <- sub x1 x3
                s <- maybeS
                sX1SubX3 <- mult s x1SubX3
                sub sX1SubX3 y1
            Just x3 = maybeX3
            Just y3 = maybeY3
addPoint (EllipticCurve a b) (Point x1 y1) (Point x2 y2)
    | x1 == x2 = Infinity
    | x1 /= x2 = Point x3 y3
        where
            maybeS = do
                t <- sub y2 y1
                b <- sub x2 x1
                divide t b
            maybeX3 = do
                s <- maybeS
                sSquare <- Just (pow s 2)
                sSquareSubX1 <- sub sSquare x1
                sub sSquareSubX1 x2
            maybeY3 = do
                s <- maybeS
                x3 <- maybeX3
                x1SubX3 <- sub x1 x3
                sX1SubX3 <- mult s x1SubX3
                sub sX1SubX3 y1
            Just x3 = maybeX3
            Just y3 = maybeY3

