module Exercises where

-- 4
fibs :: [Integer]
fibs = [x + y | (x,y) <- zip (1:fibs) (0:1:fibs)]

-- 5
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

repeatT :: a -> Tree a
repeatT x = Node (repeatT x) x (repeatT x)

takeT :: Int -> Tree a -> Tree a
takeT 0 _ = Leaf
takeT _ Leaf = Leaf
takeT n (Node l x r) = Node (takeT (n-1) l) x (takeT (n-1) r)

replicateT :: Int -> a -> Tree a
replicateT n = takeT n . repeatT
