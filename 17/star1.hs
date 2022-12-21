import Prelude hiding (Left, Right)
import Control.Monad (join)
import Data.Maybe
import Grid
{-
  This puzzle is very similar to tetris. It's even possible that the second star will involve removing fully filled lines
  in regular tetris fashion.

  The solution will thus involve:
  - Shapes in a cyclical list to support simply grabbing the next shape
  - The Board state which is a List of 7 element lists representing lines in the Board

  A round looks like
   - The shape is pushed (if possible) in the direction at the top of the directions list
   - The shape falls if it can or else is affixed in place
   - A new shape is addded if the old shape was affixed at a  point
   - The list of jets also cycles

  Each rock appears so that its left edge is two units away from the left wall and its bottom
  edge is three units above the highest rock in the room (or the floor, if there isn't one).

-}

data ShapeType = HBar | Cross | L | VBar | Square deriving (Eq, Show)
type Point = (Int, Int)
data Shape = Shape {
  points :: [Point],
  shape :: ShapeType
} deriving (Eq, Show)

data Elements = Air | Rock | FallingRock deriving (Eq)
instance Show Elements where
  show Air = "."
  show Rock = "#"
  show FallingRock = "@"

data Move = Left | Right deriving (Eq)
instance Show Move where
  show Left = "<"
  show Right = ">"

parse_move :: Char -> Move
parse_move '>' = Right
parse_move '<' = Left
parse_move ch = error ("Unexpected char '" ++ [ch] ++ "'")

intersects :: Shape -> Grid Elements -> Bool
intersects s g = any (\v -> v == Nothing || v == Just Rock) $ map (\p -> valueAtSafe p g) $ points s

shiftDown :: Shape -> Maybe Shape
shiftDown Shape { points = ps } | any (\(r, _) -> r - 1 < 0) ps = Nothing
shiftDown s@Shape { points = p } = Just (s { points = map (\(r, c) -> (r - 1, c)) p } )

shiftMove :: Shape -> Grid Elements -> Move -> Shape
shiftMove s@Shape { points = p } g move = if moved `intersects` g then s else moved
  where go col = if move == Right then (col + 1) else (col - 1)
        moved = s { points = map (\(r, c) -> (r, go c)) p }

makeRocks :: Shape -> Grid Elements -> Grid Elements
makeRocks Shape { points = ps } g = foldr (\p grid -> replace p Rock grid) g ps

getTopRock :: Grid Elements -> (Int, Int)
getTopRock = fromJust . findLastIndexWhere (== Rock)

newShape :: ShapeType -> Point -> Shape
newShape HBar (r, c) = Shape { shape = HBar, points = [(r + 4, 2), (r + 4, 3), (r + 4, 4), (r + 4, 5)]}
newShape Cross (r, c) = Shape { shape = HBar, points = [left, right, center, top, bottom]}
  -- Normally plus 3 but plus an added 1 because of the shape
  where left@(a, b) = (r + 5, 2)
        center = (a, b + 1)
        right = (a, b + 2)
        bottom = (a - 1, b + 1)
        top = (a + 1, b + 1)
newShape L (r, c) = Shape { shape = HBar, points = [left, p2, p3, p4, p5]}
  where left@(a, b) = (r + 4, 2)
        p2 = (a, b + 1)
        p3 = (a, b + 2)
        p4 = (a + 1, b + 2)
        p5 = (a + 2, b + 2)
newShape VBar (r, c) = Shape { shape = HBar, points = [(r + 4, 2), (r + 5, 2), (r + 6, 2), (r + 7, 2)]}
newShape Square (r, c) = Shape { shape = HBar, points = [bl, br, tl, tr]}
  where bl@(a, b) = (r + 4, 2)
        br = (a, b + 1)
        tl = (a + 1, b)
        tr = (a + 1, b + 1)

-- A tuple containing the current shape that's moving,
-- the current board state,
-- a list of shapes to draw new shapes from,
-- and a list of moves to make
type State = (Shape, Grid Elements, [ShapeType], [Move])

maybeExtend :: Point -> Grid Elements -> Grid Elements
maybeExtend (r, c) g = if (rowCount g) - r < 10
                          then growGridRows 10 Air g
                          else g

fall :: State -> State
fall (s, g, (next:shapes), (move:moves)) =
  if isNothing dropped || (fromJust dropped) `intersects` g
    then affixed
    else fall (fromJust dropped, g, (next:shapes), moves)
  where shifted = shiftMove s g move
        dropped = shiftDown shifted
        nextGrid = makeRocks shifted g
        topRock = getTopRock nextGrid
        nextShape = newShape next (getTopRock nextGrid)
        affixed = (nextShape, maybeExtend topRock nextGrid, shapes, moves)

run :: Int -> (a -> a) -> a -> a
run 0 f base = base
run n f base = run (n - 1) f (f base)

getGrid :: State -> Grid Elements
getGrid (_, g, _, _) = g

drawGrid :: State -> Grid Elements
drawGrid (s, grid, _, _) = foldr (\p g -> replace p FallingRock g) grid (points s)

solution :: String -> String
solution input = show . (findLastIndexWhere (== Rock)) . drawGrid $ (run count fall initial_state)
  where moves = cycle (map parse_move (join $ lines input))
        (s:shapes) = cycle [HBar, Cross, L, VBar, Square]
        count = 2022
        initial_grid = generateGrid (10, 7) Air
        initial_state = (newShape s (-1,0), initial_grid, shapes, moves)

main :: IO ()
main = interact solution
