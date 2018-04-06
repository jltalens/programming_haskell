module Exercises where

import Notes

mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult x (Succ Zero) = x
mult x (Succ y) = add x (mult x y)

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r) = case compare x y of
                           EQ -> True
                           LT -> occurs' x l
                           GT -> occurs' x r

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving Show

numLeafs :: Tree' a -> Int
numLeafs (Leaf' _) = 1
numLeafs (Node' l r) = numLeafs l + numLeafs r


balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (numLeafs l - numLeafs r) <= 1 && balanced l && balanced r


halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree' a
balance (x:[]) = Leaf' x
balance xs = Node' (balance l) (balance r) where
  l = fst halves
  r = snd halves
  halves = halve xs


data Expr' = Val' Int | Add' Expr' Expr'
folde :: (Int -> a) -> (a -> a -> a) -> Expr' -> a
folde f g expr = case expr of
  Val' n -> f n
  Add' x y -> g (folde f g x) (folde f g y)

eval' :: Expr' -> Int
eval' (Val' n) = n
eval' (Add' x y) = eval' x + eval' y

size' :: Expr' -> Int
size' (Val' _) = 1
size' (Add' x y) = size' x + size' y
