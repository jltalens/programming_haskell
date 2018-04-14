module Exercises where
--1
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- 2
{--
instance Functor ((->) a) where
  -- fmap :: (b -> c) -> (a -> b) -> (b -> c)
  fmap = (.)
--}

-- 3
{--
instance Applicative ((->) a) where
  -- pure :: b -> (a -> b)
  pure b = \a -> b
  -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
  g <*> h = \x -> g x (h x)

--}

-- 4
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  -- x -> ZipList [x, x, x...]
  pure x = Z (repeat x)

  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  -- ZipList [(a0 -> b1), (a1 -> b1), ...] -> ZipList [a0, a1, ...] -> ZipList [b0, b1, ...]
  (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]


-- 7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap _ (Val x) = Val x
  fmap f (Var e) = Var (f e)
  fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)



instance Applicative Expr where
  -- pure :: a -> Expr a
  pure = Var
  -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  _ <*> Val x = Val x
  Val x <*> _ = Val x
  Var f <*> Var a = Var (f a)
  Var f <*> Add e1 e2 = Add (fmap f e1) (fmap f e2)
  Add f g <*> x = Add (f <*> x) (g <*> x)
