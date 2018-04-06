module Exercise57 where

type Grid = [(Int, Int)]

grid :: Int -> Int -> Grid
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]


square :: Int -> Grid
square n = [(x,y) | (x, y) <- grid n n, x /= y]

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], z^2 == x^2 + y^2]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum (factors x) - x) == x]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
