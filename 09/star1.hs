import Prelude hiding (Right, Left, head, tail)
import Data.List (nub)

-- Data Defs
data Direction = Up | Down | Left | Right deriving (Show)
data Action = Action { direction :: Direction, count :: Int } deriving (Show)
type Position = (Int, Int)
data Board = Board {
  head :: Position,
  tail :: Position,
  history :: [Position]
} deriving (Show)

dist :: Num a => a -> a -> a
dist a b = abs (a - b)

resolve_tail :: Position -> Position -> Position
resolve_tail head@(hx, hy) (tx, ty) | (dist hx tx) > 1 && (dist hy ty) == 0 =
  (if tx > hx then tx - 1 else tx + 1, ty)
resolve_tail head@(hx, hy) (tx, ty) | (dist hy ty) > 1 && (dist hx tx) == 0 =
  (tx, if ty > hy then ty - 1 else ty + 1)
resolve_tail head@(hx, hy) (tx, ty) | ((dist hy ty) + (dist hx tx)) > 2 =
  (if tx > hx then tx - 1 else tx + 1, if ty > hy then ty - 1 else ty + 1)
resolve_tail head tail = tail

move_head :: Direction -> Position -> Position
move_head Up (hx, hy) = (hx, hy + 1)
move_head Down (hx, hy) = (hx, hy - 1)
move_head Left (hx, hy) = (hx - 1, hy)
move_head Right (hx, hy) = (hx + 1, hy)

step :: Action -> Board -> Board
step Action { count = 0 } board = board
step Action { direction = d, count = c } Board { head = h, tail = t, history = hist } =
  step Action { direction = d, count = c - 1 } Board {
    head = new_head,
    tail = new_tail,
    history = new_tail : hist
  }
    where new_head = move_head d h
          new_tail = resolve_tail new_head t

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
solution input = show $ (length . nub . history) $ foldl (flip step) Board { head = (0,0), tail = (0,0), history = [(0,0)]} actions
  where actions = map parse_action $ lines input

main :: IO ()
main = interact solution
