module Exercises6 where

fac :: Int -> Int
fac n
  | n <= 1 = 1
  | otherwise = n * fac (n-1)


sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

expo :: Int -> Int -> Int
expo _ 0 = 1
expo n m = n * expo n (m - 1)

euclid :: Int -> Int -> Int
euclid x y
  | x == y = x
  | otherwise = euclid a b
  where
    a = if x > y then x - y else x
    b = if y > x then y - x else y


and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = if not x then False else and' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n -1) x

(!!!) :: [a] -> Int -> a
(!!!) [] _ = error "Out of bounds"
(!!!) (x:_) 0 = x
(!!!) (_:xs) n = (!!!) xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) = if e == x then True else elem' e xs


merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x < y
  then x : merge xs (y:ys)
  else y : merge (x:xs) ys
