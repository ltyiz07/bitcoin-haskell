module Main (main) where

import Utils.Prime

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn $ show (isPrime 5)
