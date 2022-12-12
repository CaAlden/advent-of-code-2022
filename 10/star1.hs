import Data.List (unfoldr)
import Control.Monad (join)

data State = State {
  x :: Int,
  clock :: Int
} deriving (Show)

data Instruction = Instruction {
  cycles :: Int,
  name :: String
} deriving (Show)

eval :: String -> State -> State
eval "noop" State { x = currentX, clock = clk } = State { x = currentX, clock = clk + 1 }
eval ('a':'d':'d':'x':' ':args) State { x = currentX, clock = clk } = State { x = currentX + (read args), clock = clk + 2 }

solution :: String -> String
solution = show . sum . (pick 0) . (scanl (flip eval) State { x = 1, clock = 1 }) . lines
  where pick n states | n > 220 = []
        pick n states@(s:rest) | n `elem` [20, 60, 100, 140, 180, 220] = if ((clock s) <= n) then ((x s) * n) : (pick (n + 1) states) else pick n rest
        pick n states = pick (n + 1) (filter (\s -> clock s >= n) states)

main :: IO ()
main = interact solution
