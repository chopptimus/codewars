module Divisors where

divisors :: (Show a, Integral a) => a -> Either String [a]
divisors x
    | null divs = Left $ (show x) ++ " is prime"
    | otherwise = Right divs
    where divs = filter ((==) 0 . mod x) [2..(div x 2)]

main :: IO ()
main = do
    putStrLn $ show $ divisors 123
