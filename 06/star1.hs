import Data.List

find_start :: Int -> String -> Int
find_start count = go 0
  where go i [] = i
        go i xs | count == (length $ nub (take count xs)) = i + count
        go i (_:xs) = go (i + 1) xs

solution :: String -> String
solution = show . find_start 4

solution2 :: String -> String
solution2 = show . find_start 14

main :: IO ()
main = interact solution2
