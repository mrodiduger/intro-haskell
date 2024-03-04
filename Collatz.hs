module Collatz where

-- collatz sequence
f :: Int -> Int
f x
  | even x = x `div` 2
  | otherwise = 3 * x + 1

-- collatz sequence for a  given initial value
collatz :: Int -> [Int]
collatz a0 = iterate f a0

-- number of elements in the collatz sequence until the first 1
num :: Int -> Int
num m = length (takeWhile (/= 1) (collatz m))
