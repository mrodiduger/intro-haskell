module Sort where

-- insert an element into a sorted list without changing the order
insert :: Integer -> [Integer] -> [Integer]
insert x l = insertAux [] l x

insertAux :: [Integer] -> [Integer] -> Integer -> [Integer]
insertAux left right x
  | null right = left ++ [x]
  | otherwise = if x <= head right then left ++ (x : right) else insertAux (left ++ [head right]) (tail right) x

-- sort a list using insert
insertSort :: [Integer] -> [Integer]
insertSort (x : xs) = foldr insert [] (x : xs)

-- merge two sorted lists into a sorted list
merge :: [Integer] -> [Integer] -> [Integer]
merge [] b = b
merge a [] = a
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- merge sort
mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = merge (mergeSort (take (length list `div` 2) list)) (mergeSort (drop (length list `div` 2) list))