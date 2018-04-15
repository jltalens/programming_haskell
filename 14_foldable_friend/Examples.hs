module Examples where


class Monoid' a where
  mempty' :: a
  mappend' :: a -> a -> a

  mconcat' :: [a] -> a
  mconcat' = Prelude.foldr mappend' mempty'

{--
Monoid laws:
  - mempty `mappend` x          = x
  - x `mappend` mempty          = x
  - x `mappend` (y `mappend` z) = (x `mappend` y) `mappend` z
--}

instance Monoid' [a] where
  -- mempty :: [a]
  mempty' = []

  -- mappend :: [a] -> [a] -> [a]
  mappend' = (++)

instance Monoid' a => Monoid' (Maybe a) where
  -- mempty :: Maybe a
  mempty' = Nothing

  -- mappend :: Maybe a -> Maybe a -> Maybe a
  Nothing `mappend'` mb = mb
  ma `mappend'` Nothing = ma
  Just a `mappend'` Just b = Just (a `mappend'` b)

instance Monoid' Int where
  --mempty :: Int
  mempty' = 0

  -- mappend :: Int -> Int -> Int
  mappend' = (+)

{--
Ints also forms a Monoid under the * operation; but in Haskell we cannot
have multiple instance declarations of the same type for the same class
So we introduce a special-purpose wrapper type
--}

newtype Sum a = Sum a deriving (Eq, Ord, Show, Read)

getSum :: Sum a -> a
getSum (Sum x) = x

instance Num a => Monoid' (Sum a) where
  -- mempty :: Sum a
  mempty' = Sum 0

  -- mappend :: Sum a -> Sum a -> Sum a
  Sum x `mappend'` Sum y = Sum (x+y)


newtype Product a = Product a deriving (Eq, Ord, Show, Read)

getProduct :: Product a -> a
getProduct (Product a) = a

instance Num a => Monoid' (Product a) where
  -- mempty :: Product a
  mempty' = Product 1

  -- mappend :: Product a -> Product a -> Product a
  Product x `mappend'` Product y = Product (x*y)

-- x <> y = x `mappend` y


{--
 * Foldables
Combining all the values in a data structure to give a single value
--}

fold' :: Monoid' a => [a] -> a
fold' [] = mempty'
fold' (x:xs) = x `mappend'` fold' xs

data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving Show

fold'' :: Monoid' a => Tree a -> a
fold'' (Leaf x) = x
fold'' (Node l r) = fold'' l `mappend'` fold'' r

-- fold'' (Node (Leaf (Product 10)) (Leaf (Product 2)))

class Foldable' t where
  fold :: Monoid' a => t a -> a
  foldMap :: Monoid' b => (a -> b) -> t a -> b
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl :: (a -> b -> a) -> a -> t b -> a

instance Foldable' [] where
  -- fold :: Monoid a => [a] -> [a]
  fold [] = mempty'
  fold (x:xs) = x `mappend'` fold xs

  -- foldMap :: Monoid b => (a -> b) -> [a] -> b
  foldMap _ [] = mempty'
  foldMap f (x:xs) = f x `mappend'` Examples.foldMap f xs

  -- foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr _ v [] = v
  foldr f v (x:xs) = f x (Examples.foldr f v xs)

  --foldl :: (a -> b -> a) -> a -> [b] -> a
  foldl _ v [] = v
  foldl f v (x:xs) = Examples.foldl f (f v x) xs


instance Foldable' Tree where
  -- fold :: Monoid a => Tree a -> a
  fold (Leaf x) = x
  fold (Node l r) = Examples.fold l `mappend'` Examples.fold r

  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = (Examples.foldMap f l) `mappend'` (Examples.foldMap f r)

  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f v (Leaf x) = f x v
  foldr f v (Node l r) = Examples.foldr f (Examples.foldr f v r) l

  -- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl f v (Leaf x) = f v x
  foldl f v (Node l r) = Examples.foldl f (Examples.foldl f v l) r

tree :: Tree Int
tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

-- foldr (+) 0 tree = 1 + (2 + (3 + 0)
-- foldl (+) 0 tree = ((0 + 1) + 2) + 3
