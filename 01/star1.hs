 appendToLast :: a -> [[a]] -> [[a]]
 appendToLast value [] = [[value]]
 appendToLast value [x] = [(x ++ [value])]
 appendToLast value (x:xs) = x : appendToLast value xs

 group :: String -> [[String]] -> [[String]]
 group "" vals = vals ++ [[]]
 group s [] = [[s]]
 group s ys = appendToLast s ys

 sums :: [[Int]] -> [Int]
 sums = foldr (\arr all -> (sum arr) : all) []

 toInts :: [[String]] -> [[Int]]
 toInts vals = map (map read) vals

 solution :: String -> String
 solution input = show $ foldr max (0, 0) $ zip (sums $ toInts $ (foldr group) [] (lines input)) (iterate (+1) 1)

 main :: IO ()
 main = interact solution
