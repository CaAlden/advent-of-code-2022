{-
  1. Find the duplicate element in each line of the input
  2. Map that element to a value
  3. Sum the values
-}

import Data.Set hiding (map, split, drop, take)
import qualified Data.Char as Char

priority :: Char -> Int
priority ch | ch >= 'a' && ch <= 'z' = 1 + (Char.ord ch) - (Char.ord 'a')
priority ch = 27 + (Char.ord ch) - (Char.ord 'A')

shared :: (String, String, String) -> Char
shared (a, b, c) = head $ elems $
  intersection (fromList c) $
    intersection (fromList a) $ (fromList b)

group3 :: [String] -> [(String, String, String)]
group3 [] = []
group3 (a:b:c:rest) = (a,b,c) : group3 rest

solution :: String -> String
solution input = show $ sum $ map priority $ map shared $ group3 $ lines input

main :: IO ()
main = interact solution
