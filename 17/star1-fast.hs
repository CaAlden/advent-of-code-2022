import Prelude hiding (Left, Right)
import Control.Monad (join)
import Data.Maybe
import Grid
{-
   The only difference between this and star 1 is that the number of rocks
   is rediculous. But you don't actually need to track that much memory,
   just the lowest level where no other rocks can possibly get through

   Even then I don't think you have to be that careful, just store the last 100
   lines or so and keep track of how many lines have been removed
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
-- The amount the board has been offset by
-- a list of shapes to draw new shapes from,
-- and a list of moves to make
type State = (Shape, Grid Elements, Int, [ShapeType], [Move])

maybeExtend :: Point -> Grid Elements -> Grid Elements
maybeExtend (r, c) g = if (rowCount g) - r < 10
                          then growGridRows 10 Air g
                          else g

maybeTrim :: Grid Elements -> (Int, Grid Elements)
maybeTrim g = if (rowCount g) > 200
                 then (100, dropGridRows 100 g)
                 else (0, g)

fall :: State -> State
fall (s, g, offset, (next:shapes), (move:moves)) =
  if isNothing dropped || (fromJust dropped) `intersects` g
    then affixed
    else fall (fromJust dropped, g, offset, (next:shapes), moves)
  where shifted = shiftMove s g move
        dropped = shiftDown shifted
        nextGrid = makeRocks shifted g
        topRock = getTopRock nextGrid
        (newOffset, finalizedAffixedGrid) = maybeTrim $ maybeExtend topRock nextGrid
        nextShape = newShape next topRock
        correctedNextShape = nextShape { points = map (\(r, c) -> (r - newOffset, c)) (points nextShape) }
        affixed = (correctedNextShape, finalizedAffixedGrid, offset + newOffset, shapes, moves)

run :: Int -> (a -> a) -> a -> a
run 0 f base = base
run n f base = run (n - 1) f (f base)

getGrid :: State -> Grid Elements
getGrid (_, g, _, _, _) = g

getOffset :: State -> Int
getOffset (_, _, offset, _, _) = offset

drawGrid :: State -> Grid Elements
drawGrid (s, grid, _, _, _) = foldr (\p g -> replace p FallingRock g) grid (points s)

solution :: String -> String
solution input = show $ fmap ((+ (finalOffset + 1)) . fst) $ findLastIndexWhere (== Rock) finalGrid
  where moves = cycle (map parse_move (join $ lines input))
        (s:shapes) = cycle [HBar, Cross, L, VBar, Square]
        -- count = 1000000000000
        count = 10000
        initial_grid = generateGrid (10, 7) Air
        initial_state = (newShape s (-1,0), initial_grid, 0, shapes, moves)
        finalState = (run count fall initial_state)
        finalOffset = getOffset finalState
        finalGrid = getGrid finalState

main :: IO ()
main = interact solution
