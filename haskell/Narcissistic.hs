module Narcissistic where

sumOfDigitSquares 0 _ sum = sum
sumOfDigitSquares x i sum = sumOfDigitSquares (quot x 10) i (sum + ((mod x 10) ^ i))

narcissistic :: Integral n => n -> Bool
narcissistic x = x == sumOfDigitSquares x (ceiling $ logBase 10 $ fromIntegral x) 0

main :: IO ()
main = do
    putStrLn $ show $ narcissistic 153
