module Merge where

-- stream of odd numbers
odds :: [Integer]
odds = 1 : map (+ 2) odds

-- stream of odd primes
oddPrimes :: [Integer] -> [Integer]
oddPrimes (p : ps) = p : oddPrimes [a | a <- ps, a `mod` p /= 0]

-- stream of primes
primes :: [Integer]
primes = 2 : oddPrimes (tail odds)

-- merge two sorted lists
merge :: [Integer] -> [Integer] -> [Integer]
merge [] b = b
merge a [] = a
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- primepowers computes the stream of first n powers of all primes in increasing order
-- using only primes and merge
primepowers :: Integer -> [Integer]
primepowers n = foldr merge [] [map (^ i) primes | i <- [1 .. n]]