import Prelude hiding (Right, Left)
import Data.List (nub, replicate)

-- Data Defs
data Direction = Up | Down | Left | Right deriving (Show)
data Action = Action { direction :: Direction, count :: Int } deriving (Show)
type Position = (Int, Int)
data Knot = Knot {
  pos :: Position,
  history :: [Position]
} deriving (Show)

dist :: Num a => a -> a -> a
dist a b = abs (a - b)

resolve_t :: Position -> Position -> Position
resolve_t h@(hx, hy) (tx, ty) | (dist hx tx) > 1 && (dist hy ty) == 0 =
  (if tx > hx then tx - 1 else tx + 1, ty)
resolve_t h@(hx, hy) (tx, ty) | (dist hy ty) > 1 && (dist hx tx) == 0 =
  (tx, if ty > hy then ty - 1 else ty + 1)
resolve_t h@(hx, hy) (tx, ty) | ((dist hy ty) + (dist hx tx)) > 2 =
  (if tx > hx then tx - 1 else tx + 1, if ty > hy then ty - 1 else ty + 1)
resolve_t h t = t

move_h :: Direction -> Position -> Position
move_h Up (hx, hy) = (hx, hy + 1)
move_h Down (hx, hy) = (hx, hy - 1)
move_h Left (hx, hy) = (hx - 1, hy)
move_h Right (hx, hy) = (hx + 1, hy)

move_knots :: Direction -> [Knot] -> [Knot]
move_knots dir knots = go (moved_head:rest)
  where
    first = head knots
    rest = tail knots
    new_head_pos = move_h dir (pos first)
    moved_head = first { pos = new_head_pos, history = new_head_pos : (history first) }
    go (x:[]) = x:[]
    go (prev:curr:rest) = prev : go (new_curr:rest)
      where moved_pos = resolve_t (pos prev) (pos curr)
            new_curr = Knot { pos = moved_pos, history = moved_pos : (history curr) }

step :: Action -> [Knot] -> [Knot]
step Action { count = 0 } knot = knot
step Action { direction = d, count = c } knots = step Action { direction = d, count = c - 1} (move_knots d knots)

-- Parsing

parse_direction :: String -> Direction
parse_direction ('U':_) = Up
parse_direction ('D':_) = Down
parse_direction ('L':_) = Left
parse_direction ('R':_) = Right

parse_action :: String -> Action
parse_action (d:' ':c) = Action { direction = parse_direction (d:[]), count = read c }

-- IO

solution :: String -> String
solution input = (show $ (length . nub . history) $ last final_knots) ++ "\n"
  where actions = map parse_action $ lines input
        inital_knots = replicate 10 Knot { pos = (0,0), history = [(0,0)] }
        final_knots = foldl (flip step) inital_knots actions

main :: IO ()
main = interact solution
