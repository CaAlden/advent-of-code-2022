import Data.List (transpose, intercalate)

set_visible :: [Int] -> [Bool]
set_visible input = go (-1) input
  where go largest [] = []
        go largest (t:ts) | t > largest = True : (go t ts)
        go largest (_:ts) = False : (go largest ts)

find_visible_trees :: [[Int]] -> [[Bool]]
find_visible_trees = map set_visible

-- Helpers for collecting the mappings together

combine :: (a -> a -> b) -> [a] -> [a] -> [b]
combine _ [] [] = []
combine combinator (a:as) (b:bs) = (combinator a b) : combine combinator as bs

combine_maps :: [Bool] -> [Bool] -> [Bool]
combine_maps = combine (||)

combine_grid :: [[Bool]] -> [[Bool]] -> [[Bool]]
combine_grid = combine combine_maps

combine_many_grids :: [[[Bool]]] -> [[Bool]]
combine_many_grids = foldr1 combine_grid

count_visible :: [[Bool]] -> Int
count_visible = sum . map (length . filter id)

-- End of helpers

parse :: String -> [[Int]]
parse i = map (map read) chars
  where chars = map (map (:[])) (lines i)

pretty_print bs = intercalate "\n" (map (map p) bs)
  where p True = 'T'
        p False = 'F'

solution :: String -> String
solution i = show $ count_visible $ combine_many_grids $ reverted_views
  where grid = parse i
        views = [grid, map reverse grid, transpose grid, map reverse (transpose grid)]
        (a:b:c:d:[]) = map find_visible_trees views
        reverted_views = [a, map reverse b, transpose c, transpose (map reverse d)]



main :: IO ()
main = interact solution
