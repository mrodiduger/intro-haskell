module Ropes where

-- rope data structure
data Rope a
  = Leaf [a]
  | Inner (Rope a) Int (Rope a)

-- length of a rope
ropeLength :: Rope a -> Int
ropeLength (Leaf s) = length s
ropeLength (Inner _ w r) = w + ropeLength r

-- concatenate two ropes
ropeConcat :: Rope a -> Rope a -> Rope a
ropeConcat l r = Inner l (ropeLength l) r

-- split a rope into a tuple of ropes at a given position
ropeSplitAt :: Int -> Rope a -> (Rope a, Rope a)
ropeSplitAt i (Leaf s) = (Leaf (take i s), Leaf (drop i s))
ropeSplitAt i (Inner l w r)
  | i < w = let (ll, lr) = ropeSplitAt i l in (ll, ropeConcat lr r)
  | i > w = let (rl, rr) = ropeSplitAt (i - w) r in (ropeConcat l rl, rr)
  | otherwise = (l, r)

-- insert a rope into another rope at a given position
ropeInsert :: Int -> Rope a -> Rope a -> Rope a
ropeInsert i a b = let (l, r) = ropeSplitAt i b in ropeConcat (ropeConcat l a) r

-- delete a substring from a rope
ropeDelete :: Int -> Int -> Rope a -> Rope a
ropeDelete i j rope =
  let (il, _) = ropeSplitAt i rope
      (_, jr) = ropeSplitAt j rope
   in ropeConcat il jr
