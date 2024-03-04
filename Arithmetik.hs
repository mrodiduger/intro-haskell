module Arithmetik where

-- naive implementation of pow
pow1 :: Integer -> Integer -> Integer
pow1 b 0 = 1
pow1 b e = b * pow1 b (e - 1)

-- a more efficient implementation of pow
pow2 :: Integer -> Integer -> Integer
pow2 b 0 = 1
pow2 b e
  | even e = pow2 (b * b) (e `div` 2)
  | otherwise = b * pow2 b (e - 1)

-- an implementation of pow using an accumulator
pow3_acc :: Integer -> Integer -> Integer -> Integer
pow3_acc b e x = if e == 0 then x else pow3_acc b (e - 1) (x * b)

pow3 :: Integer -> Integer -> Integer
pow3 b e = if e >= 0 then pow3_acc b e 1 else error "Exponent can not be negative!"

-- root function that calculates the x ,e-th integer root of r, s.t. x^e <= r < (x+1)^e
root :: Integer -> Integer -> Integer
root e r
  | e == 0 = 1
  | e == 1 = r
  | otherwise = rootAux 0 r e r

rootAux :: Integer -> Integer -> Integer -> Integer -> Integer
rootAux a b e r
  | b - a == 1 = a
  | otherwise = if (b - (b - a) `div` 2) ^ e <= r then rootAux (b - (b - a) `div` 2) b e r else rootAux a (b - (b - a) `div` 2) e r

-- a function that checks if a number is prime
isPrime :: Integer -> Bool
isPrime x = not (any (\a -> x `mod` a == 0) [2 .. (root 2 x)])