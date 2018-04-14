module Examples where

-- * FUNCTORS
class Functor' f where
  fmap :: (a -> b) -> f a -> f b

{--
* Functor' [] is a more primitive way of defining a Functor' [a]
* The comment in fmap is because Haskell doesn't allow type information inside Instances
--}

instance Functor' [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap = map

data Maybe' a = Nothing' | Just' a deriving Show

instance Functor' Maybe' where
  -- fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap _ Nothing' = Nothing'
  fmap g (Just' x) = Just' (g x)


data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving Show

instance Functor' Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (Examples.fmap g l) (Examples.fmap g r)


{--
 * Functor' f: 'f' usually f is a container type (f is a data structure that contains elements of a)
   but is not always the case; for example IO.
--}

instance Functor' IO where
  -- fmap :: (a -> b) -> IO a -> IO b
  fmap g mx = do {x <- mx; return (g x)}


{--
Functor laws:
   * fmap id = id
   * fmap (g . h) = fmap g . fmap h
--}


-- * APPLICATIVES

{--
Abstract the idea of a functor to allow functions with any number of parameters
* fmap0 :: a -> f a
* fmap1 :: (a -> b) -> f a -> f b
* fmap2 :: (a -> b -> c) -> f a -> f b -> f c
...

Using the idea of currying we can constuct
* pure :: a -> f a
* (<*>) :: f (a -> b) -> f a -> f b
so:
fmap0 = pure
fmap1 g x = pure g <*> x
fmap2 g x y = pure g <*> x <*> y
--}

class Functor' f => Applicative' f where
  pure' :: a -> f a
  (<**>) :: f (a -> b) -> f a -> f b

instance Applicative' Maybe' where
  -- pure :: a -> f a
  pure' = Just'
  -- (<*>) :: f (a -> b) -> f a -> f b
  Nothing' <**> _ = Nothing'
  (Just' g) <**> mx = Examples.fmap g mx


instance Applicative' [] where
  -- pure :: a -> [a]
  pure' x = [x]
  -- (<*>) :: [a -> b] -> [a] -> [b]
  gs <**> ms = [g x | g <- gs, x <- ms]

instance Applicative' IO where
  -- pure :: a -> IO a
  pure' = return
  -- (<*>) :: IO (a -> b) -> IO a -> IO b
  mg <**> mx = do {g <- mg; x <- mx; return (g x)}

getChars' :: Int -> IO String
getChars' 0 = return []
getChars' n = pure' (:) <**> getChar <**> getChars' (n-1)


-- We can generalise on this idea
sequenceA' :: Applicative' f => [f a] -> f [a]
sequenceA' [] = pure' []
sequenceA' (x:xs) = pure' (:) <**> x <**> sequenceA' xs

getChars'' :: Int -> IO String
getChars'' n = sequenceA' (replicate n getChar)

{--
Applicative laws:
 * pure id <*> x = x
 * pure (g x) = pure g <*> pure x
 * x <*> pure y = pure (\g -> g y) <*> x
 * x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
--}

-- pure g <*> x == g <$> x     g <$> x1 <*> x2 <*> x3 ... <*> xn


-- * MONADS
data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

-- Can fail in a div by zero expection
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

eval' :: Expr -> Maybe Int
eval' (Val n) = Just n
eval' (Div x y) = case eval' x of
                    Nothing -> Nothing
                    Just n -> case eval' y of
                      Nothing -> Nothing
                      Just m -> safediv n m

{--
How to rewrite this in a simpler manner?
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>= f = case mx of
                Nothing -> Nothing
                Just x -> f x

(>>=) is often called bind
--}

eval'' :: Expr -> Maybe Int
eval'' (Val n) = Just n
eval'' (Div x y) = do n <- eval'' x
                      m <- eval'' y
                      safediv n m

-- the 'do' notation can be used in any Monad

class Applicative' m => Monad' m where
  return' :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

  return' = pure'


instance Monad' Maybe' where
  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing' >>= _ = Nothing'
  (Just' x) >>= f = f x


instance Monad' [] where
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  xs >>= f = [y | x <- xs, y <- f x]

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x, y)

{--
Monad laws:
* return x >>= f = f x
* mx >>= return = mx
* (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))
--}
