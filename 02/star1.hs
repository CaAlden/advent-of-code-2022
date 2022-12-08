{-
  Scoring:
  You get points based on what you chose to play:
    Rock = 1
    Paper = 2
    Scissors = 3
  ...as well as the outcome
    Loss = 0
    Draw = 3
    Win = 6

  You have to figure out the total points scored based on the input strategy guide
-}

import Data.List

splitOn :: Eq a => (a -> Bool) -> [a] -> [[a]]
splitOn f [] = []
splitOn f xs =
  let (group, rest) = break f xs
      remainder | (rest == []) = []
                | otherwise    = tail rest
  in group : splitOn f remainder

points :: String -> Int
points x | isInfixOf "X" x = 1
points x | isInfixOf "Y" x = 2
points x | isInfixOf "Z" x = 3
points _ = 0

score :: String -> Int
score x | x `elem` ["A Y", "B Z", "C X"] = 6 + (points x)
score x | x `elem` ["A X", "B Y", "C Z"] = 3 + (points x)
score x | x `elem` ["A Z", "B X", "C Y"] = 0 + (points x)
score _ = error "Invalid input"

solution :: String -> String
solution s = show $ sum $ map score $ lines s

main :: IO ()
main = interact solution
