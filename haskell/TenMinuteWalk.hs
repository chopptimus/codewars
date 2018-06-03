module Codewars.Kata.TenMinuteWalk where

isValidWalk' :: [Char] -> Int -> Int -> Int -> Bool
isValidWalk' [] l x y = l == 10 && x == 0 && y == 0
isValidWalk' (c:cs) l x y
    | l > 10 = False
    | c == 'n' = isValidWalk' cs (l + 1) (x + 1) y
    | c == 's' = isValidWalk' cs (l + 1) (x - 1) y
    | c == 'w' = isValidWalk' cs (l + 1) x (y + 1)
    | c == 'e' = isValidWalk' cs (l + 1) x (y - 1)
    | otherwise = False

isValidWalk :: [Char] -> Bool
isValidWalk walk = isValidWalk' walk 0 0 0

main :: IO ()
main = do
    putStrLn $ show $ isValidWalk (repeat 'n')
