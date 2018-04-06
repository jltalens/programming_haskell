module Notes where

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

type Pos = (Int, Int)
data Move = North | South | East | West deriving Show

move :: Move -> Pos -> Pos
move North (x, y) = (x, y+1)
move South (x, y) = (x, y-1)
move East (x, y) = (x+1, y)
move West (x, y) = (x-1, y)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

-- Single constructor with single argument
newtype Nato = N Int
{--
Alternative to newtype is:
type Nato = Int
data Nato = N Int
but; with newtype Nat and Int are not mixedup => more type safety
more efficient as the constructor is removed at compile type once
type safety has been guaranteed.
--}

data Nat = Zero | Succ Nat deriving Show
-- Succ ( Succ ( Succ Zero ) )
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)


data List' a = Nil | Cons a (List' a)

len :: List' a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
    (Node (Leaf 6) 7 (Leaf 8))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r
