import Data.List (transpose, intercalate)

parse :: String -> [[Int]]
parse i = map (map read) chars
  where chars = map (map (:[])) (lines i)

pretty_print bs = intercalate "\n" (map (map p) bs)
  where p True = 'T'
        p False = 'F'

scenic_score_helper :: ([Int], [Int], [Int], [Int]) -> Int -> Int
scenic_score_helper (left, right, down, up) height = foldl1 (*) [go left, go right, go up, go down]
  where go [] = 0
        go (x:xs) | x < height = 1 + (go xs)
        go _ = 1

scenic_score :: [[Int]] -> (Int, Int) -> Int
scenic_score grid (x, y) = scenic_score_helper (left, right, down, up) ((grid!!y)!!x)
  where left = (get_left grid x y)
        right = (get_right grid x y)
        up = (get_up grid x y)
        down = (get_down grid x y)

get_left :: [[Int]] -> Int -> Int -> [Int]
get_left grid x y = reverse (take x (grid!!y))

get_right :: [[Int]] -> Int -> Int -> [Int]
get_right grid x y = drop (x + 1) (grid!!y)

get_up :: [[Int]] -> Int -> Int -> [Int]
get_up grid x y = get_left (transpose grid) y x

get_down :: [[Int]] -> Int -> Int -> [Int]
get_down grid x y = get_right (transpose grid) y x


solution :: String -> String
solution i = show $ maximum $ scores
  where grid = parse i
        scores = map (scenic_score grid) coords
        coords = [(x, y) | x <- [0..((length (grid!!0)) - 1)], y <- [0..((length grid) - 1)]]

main :: IO ()
main = interact solution
