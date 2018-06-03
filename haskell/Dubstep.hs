module Codewars.Kata.Dubstep where
import Data.List

decode :: [Char] -> Bool -> [Char] -> [Char]
decode [] _ [] = []
decode [] _ s = tail $ reverse s
decode remix b original
    | Just rest <- stripPrefix "WUB" remix = decode rest True original
    | b = decode (tail remix) False (head remix:' ':original)
    | not b = decode (tail remix) False (head remix:original)

songDecoder :: [Char] -> [Char]
songDecoder string = decode string True []

main :: IO ()
main = do
    putStrLn $ songDecoder "WUBWEWUBAREWUBWUBTHEWUBCHAMPIONSWUBMYWUBFRIENDWUB"
