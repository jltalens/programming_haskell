module Exercises where

import Data.Char (digitToInt)

-- 1
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

-- 2 and 3
type Board = [Int]

initial :: Board
initial = [7,6,5,4,3,2,1]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard b = sequence_ [putBoard_ b r | r <- [0..(length b) -1]]

putBoard_ :: Board -> Int -> IO ()
putBoard_ b s = do putRow (s+1) (b !! s)

-- 4

adderTotal :: Int -> Int -> IO ()
adderTotal 0 acc = do putStrLn ("The total is " ++ (show acc))
adderTotal i acc = do
  x <- getLine
  adderTotal (i-1) (acc + (read x))

adder :: IO ()
adder = do
  _ <- putStr "How many numbers? "
  x <- getLine
  _ <- adderTotal (read x) 0
  return ()
