module Exercise78 where

exer1 :: Eq a => (a -> b) -> (a -> Bool) -> [a] -> [b]
exer1 f p = map f . filter p

all' :: (a -> Bool) -> [a] -> Bool
all' f xs = length (filter f xs) == length xs

any' :: (a -> Bool) -> [a] -> Bool
any' f xs = length (filter f xs) > 0

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) = case f x of
  True -> x : takeWhile' f xs
  False -> []


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs) = case f x of
  True -> dropWhile' f xs
  False -> (x:xs)


mapAsFold :: (a -> b) -> [a] -> [b]
mapAsFold f = foldr (\x acc -> f x : acc) []

filterAsFold :: (a -> Bool) -> [a] -> [a]
filterAsFold f = foldr (\x acc -> if f x then x : acc else acc) []

{--
dec2Int :: [Int] -> Int
dec2Int = foldl (\acc x -> acc + snd x * (fst x) * 10) 0 . zip [0..] . reverse
--}

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) ->(a, b) -> c
uncurry' f p = f (fst p) (snd p)

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f1 f2 (x:xs) = f1(x) : altMap f2 f1 xs
