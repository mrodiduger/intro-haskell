module ChurchNumbers where

-- church numbers
type Church t = (t -> t) -> t -> t

-- convert an integer to a church number
int2church :: Integer -> Church t
int2church i
  | i == 0 = \f x -> x
  | otherwise = \f x -> f (int2church (i - 1) f x)

-- convert a church number to an integer
church2int :: Church Integer -> Integer
church2int c = c (+ 1) 0