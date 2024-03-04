module Fibs where

-- infinite list of fibonacci numbers
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)