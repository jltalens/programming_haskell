module Exercises where
import Examples
-- 1
instance (Monoid' a, Monoid' b) => Monoid' (a,b) where
  -- mempty :: (a, b)
  mempty' = (mempty', mempty')

  -- mappend :: (a,b) -> (a,b) -> (a,b)
  (a1,b1) `mappend'` (a2,b2) = (a1 `mappend'` a2, b1 `mappend'` b2)

-- 2
instance Monoid' b => Monoid' (a -> b) where
  -- mempty :: (a -> b)
  mempty' = \_ -> mempty'

  -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
  f `mappend'` g = \x -> f x `mappend'` g x

-- 3
instance Foldable' Maybe where
  -- fold :: Monoid a => Maybe a -> a
  fold (Just x) = x
  fold Nothing = mempty'

  -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
  foldMap _ Nothing = mempty'
  foldMap f (Just x) = f x

  -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr _ v Nothing = v
  foldr f v (Just x) = f x v

  -- foldl :: (a -> b -> a) -> a -> Maybe b -> a
  foldl _ v Nothing = v
  foldl f v (Just x) = f v x

instance Traversable' Maybe where
  -- traverse :: (a -> f b) -> Maybe a = f Maybe b
  traverse' _ Nothing = pure Nothing
  traverse' g (Just x) = pure Just <*> g x

-- 4
data BinTree a = BinLeaf | BinNode (BinTree a) a (BinTree a)
               deriving Show

instance Foldable' BinTree where
  -- fold :: Monoid a => BindTree a -> a
  fold BinLeaf = mempty'
  fold (BinNode l x r) = Examples.fold l `mappend'` x `mappend'` (Examples.fold r)

  -- foldMap :: Monoid b = (a -> b) -> BinTree a -> BinTree b
  foldMap _ BinLeaf = mempty'
  foldMap f (BinNode l x r) = (Examples.foldMap f l) `mappend'` f x `mappend'` (Examples.foldMap f r)

  -- foldr :: (a -> b -> b) -> b -> Bintree a -> binTree b
  foldr _ v BinLeaf = v
  foldr f v (BinNode l x r) = Examples.foldr f (f x (Examples.foldr f v r)) l

  -- foldl :: (a -> b -> a) -> a -> BinTree b -> a
  foldl _ v BinLeaf = v
  foldl f v (BinNode l x r) = Examples.foldl f (f (Examples.foldl f v l) x) r

instance Functor BinTree where
  -- fmap :: (a -> b) -> BinTree a -> BinTree b
  fmap _ BinLeaf = BinLeaf
  fmap f (BinNode l x r) = BinNode (fmap f l) (f x) (fmap f r)


instance Traversable' BinTree where
  -- traverse :: (a -> f b) -> BinTree a -> f BinTree b
  traverse' _ BinLeaf = pure BinLeaf
  traverse' g (BinNode l x r) = pure BinNode <*> traverse' g l <*> g x <*> traverse' g r
