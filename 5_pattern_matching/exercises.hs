module Exercises48 where


halve :: [a] -> ([a], [a])
halve xs = foldl f z xs where
  z = ([], [])
  f = \(as, bs) x -> if length as < length xs `div` 2
    then (as ++ [x], bs)
    else (as, bs ++ [x])


third' :: [a] -> a
third' xs = head (tail ( tail xs ))

third'' :: [a] -> a
third'' xs = xs !! 3

third''' :: [a] -> a
third''' (_:_:x:_) = x

safetail' :: [a] -> [a]
safetail' xs = if null xs then [] else tail xs

safetail'' :: [a] -> [a]
safetail'' xs | null xs = []
              | otherwise = tail xs

safetail''' :: [a] -> [a]
safetail''' [] = []
safetail''' xs = tail xs

luhnDouble :: Int -> Int
luhnDouble i = f (i * 2) where
  f = \x -> if x > 9 then x-9 else x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn d1 d2 d3 d4 = (doubles `mod` 10 == 0)  where
  doubles = sum [luhnDouble d1, d2, luhnDouble d3, d4]
