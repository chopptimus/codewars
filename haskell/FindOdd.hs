module Codewars.Kata.FindOdd where

findOdd :: [Int] -> Int
findOdd xs = head $ filter (oddCount xs) xs 

oddCount xs y = odd $ count' xs y 0
    where count' [] _ i = i
          count' (x:xs) y i
            | x == y = count' xs y (i + 1)
            | otherwise = count' xs y i
