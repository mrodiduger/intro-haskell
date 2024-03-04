module Polynom where

-- polynom represented as a list of coefficients
type Polynom = [Double]

-- multiply a polynom by a constant
cmult :: Polynom -> Double -> Polynom
cmult [] c = []
cmult p c = map (* c) p

-- evaluate a polynom at a given point
eval :: Polynom -> Double -> Double
eval [] x = 0
eval p x = foldr (\a b -> a + x * b) 0 p

-- derive a polynom
deriv :: Polynom -> Polynom
deriv p = zipWith (*) [1 ..] (tail p)