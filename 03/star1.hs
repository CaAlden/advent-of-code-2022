{-
  1. Find the duplicate element in each line of the input
  2. Map that element to a value
  3. Sum the values
-}

import Data.Set hiding (map, split, drop, take)
import qualified Data.Char as Char

substring :: Int -> Int -> String -> String
substring start end str = take (end - start) $ drop start str

split :: String -> (String, String)
split s = ((substring 0 mid s), (substring mid (length s) s))
  where mid = (length s) `div` 2

priority :: Char -> Int
priority ch | ch >= 'a' && ch <= 'z' = 1 + (Char.ord ch) - (Char.ord 'a')
priority ch = 27 + (Char.ord ch) - (Char.ord 'A')

shared :: (String, String) -> Char
shared (a, b) = head $ elems $ intersection (fromList a) $ (fromList b)

solution :: String -> String
solution input = show $ sum $ map priority $ map (shared . split) $ lines input

main :: IO ()
main = interact solution
