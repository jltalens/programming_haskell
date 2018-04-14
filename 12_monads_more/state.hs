module State where

import Data.Char (isDigit,digitToInt)

type State = Int

{--
type ST a = State -> (a,State)
Given that ST is a parameterised type is natural to try and make it a monad so we can
use the 'do' notation. However, types declared with 'type' cannot be made into instances
of classes
--}

newtype ST a = S (State -> (a,State))

-- Function to remove the dummy 'S' constructor introduced by the 'newtype' operator
app :: ST a -> State -> (a, State)
app (S st) x = st x


instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

-- 'let' is similar to 'where' but allows local definitions in expressions rather than in functions


instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S (\s ->
                     let (f, s') = app stf s
                         (x, s'') = app stx s' in (f x, s'')
                  )

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')


-- Relabelling trees

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- problem : relabel each leaf with a unique integer.
-- First approach

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l', n') = rlabel l n
    (r', n'') = rlabel r n'

-- We need to carry the integer through the computation. Second approach with State

fresh :: ST Int
fresh = S (\n -> (n, n+1))

-- Being that we have an Applicative instance of ST:
alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

-- Given that we have a Monad instance as well:
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do n <- fresh
                     return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')

-- Generic functions over monads

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ [] = return []
mapM' f (x:xs) = do y <- f x
                    ys <- mapM' f xs
                    return (y:ys)

conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x:xs) = do b <- p x
                      ys <- filterM p xs
                      return (if b then x:ys else ys)
-- Building a powerset : filterM (\x -> [True,False]) [1,2,3]

join :: Monad m => m (m a) -> m a
join mmx = do mx <- mmx
              x <- mx
              return x
