import Grid
import Control.Monad (join)

type Point = (Int, Int)
type Line = (Point, Point)

data Element = Sand | Air | Rock | Source deriving (Eq)

instance Show Element where
  show Sand = "o"
  show Air = "."
  show Rock = "#"
  show Source = "x"

draw_line :: Element -> Line -> Grid Element -> Grid Element
draw_line e ((x, y), (x2, y2)) grid | x == x2 && y == y2 = replace (x, y) e grid
draw_line e ((x, y), (x2, y2)) grid | x == x2 && y > y2 = draw_line e ((x, y - 1), (x2, y2)) (replace (x, y) e grid)
draw_line e ((x, y), (x2, y2)) grid | x == x2 && y < y2 = draw_line e ((x, y + 1), (x2, y2)) (replace (x, y) e grid)
draw_line e ((x, y), (x2, y2)) grid | y == y2 && x > x2 = draw_line e ((x - 1, y), (x2, y2)) (replace (x, y) e grid)
draw_line e ((x, y), (x2, y2)) grid | y == y2 && x < x2 = draw_line e ((x + 1, y), (x2, y2)) (replace (x, y) e grid)
draw_line e ((x, y), (x2, y2)) grid | y < y2 && x < x2 = draw_line e ((x + 1, y + 1), (x2, y2)) (replace (x, y) e grid)
draw_line e ((x, y), (x2, y2)) grid | y > y2 && x > x2 = draw_line e ((x - 1, y - 1), (x2, y2)) (replace (x, y) e grid)

get_bounds :: [Point] -> (Point, Point)
get_bounds ls = (tl, br)
  where adjust_max (x, y) (a, b) = (max x a, max y b)
        adjust_min (x, y) (a, b) = (min x a, min y b)
        tl = foldl1 adjust_min ls
        br = foldl1 adjust_max ls

make_lines :: [Point] -> [Line]
make_lines [] = []
make_lines (p1:p2:[]) = [(p1, p2)]
make_lines (p1:p2:rest) = (p1, p2) : (make_lines (p2:rest))

parse_line :: String -> [Line]
parse_line s = make_lines (map read tups)
  where parts = filter (/= "->") (words s)
        tups = map (\s -> "(" ++ s ++ ")") parts

parse_input_lines :: String -> [Line]
parse_input_lines = join . map parse_line . lines

points :: [Line] -> [Point]
points [] = []
points ((a,b):xs) = [a, b] ++ points xs

is_outside :: Point -> Grid Element -> Bool
is_outside (x, y) g = x < 0 || x >= rowCount g || y < 0 || y >= columnCount g

is_filled :: Point -> Grid Element -> Bool
is_filled p g | is_outside p g = False
is_filled p g = value == Sand || value == Rock
  where value = valueAt p g

fill_with_sand :: Point -> Point -> Grid Element -> Grid Element
fill_with_sand source p g | is_outside p g = g
fill_with_sand source p g | source == p && is_filled p g = g
fill_with_sand source (r, c) g | not $ is_filled (r + 1, c) g = fill_with_sand source (r + 1, c) g
fill_with_sand source (r, c) g | not $ is_filled (r + 1, c - 1) g = fill_with_sand source (r + 1, c - 1) g
fill_with_sand source (r, c) g | not $ is_filled (r + 1, c + 1) g = fill_with_sand source (r + 1, c + 1) g
fill_with_sand source p g | not $ is_filled p g = fill_with_sand source source $ replace p Sand g

solution :: String -> String
solution input = show sand_count ++ "\n"
  where ls = parse_input_lines input
        bounds = get_bounds $ points ls ++ [(500, 0)]
        left = fst $ fst bounds
        top = snd $ fst bounds
        right = (fst $ snd bounds)
        bottom = snd $ snd bounds
        rows = bottom + 3 -- Add additional rows for the infinite bottom line
        columns = (right - left) + 1 + (bottom * 2) -- Pad out to the left and right a distance twice the height of the grid
        source = (0 - top, (500 - left) + bottom) -- + bottom recenters the source
        bottom_line = ((rows - 1, 0), (rows - 1, columns - 1))
        fixed_lines = map (\((x, y), (x2, y2)) -> ((y - top, (x - left) + bottom), (y2 - top, (x2 - left) + bottom))) ls
        all_lines = bottom_line:fixed_lines
        empty_grid = replace source Source $ generateGrid (rows, columns) Air
        initial_grid = foldr (draw_line Rock) empty_grid all_lines
        filled = fill_with_sand source source initial_grid
        sand_count = foldr (\cell acc -> (if cell == Sand then 1 else 0) + acc) 0 filled

main :: IO ()
main = interact solution
