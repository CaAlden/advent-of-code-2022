import Data.List

appendToLast :: a -> [[a]] -> [[a]]
appendToLast value [] = [[value]]
appendToLast value [x] = [(x ++ [value])]
appendToLast value (x:xs) = x : appendToLast value xs

myGroup :: String -> [[String]] -> [[String]]
myGroup "" vals = vals ++ [[]]
myGroup s [] = [[s]]
myGroup s ys = appendToLast s ys

sums :: [[Int]] -> [Int]
sums = foldr (\arr all -> (sum arr) : all) []

toInts :: [[String]] -> [[Int]]
toInts vals = map (map read) vals

solution :: String -> String
solution input = show $ sum $ take 3 $ reverse $ sort $ sums $ toInts $ (foldr myGroup) [] (lines input)

main :: IO ()
main = interact solution
