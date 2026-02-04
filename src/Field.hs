module Field
    ( Field(..)
    ) where

class Eq a => Field a where
    add :: a -> a -> Maybe a
    sub :: a -> a -> Maybe a
    mult :: a -> a -> Maybe a
    divide :: a -> a -> Maybe a
    pow :: a -> Integer -> a 
    inv :: a -> Maybe a
    fromIntWith :: a -> Integer -> a
