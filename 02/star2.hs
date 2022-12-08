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
points "A" = 1
points "B" = 2
points "C" = 3

loseTo :: String -> String
loseTo x | isInfixOf "A" x = "C"
loseTo x | isInfixOf "B" x = "A"
loseTo x | isInfixOf "C" x = "B"

drawTo :: String -> String
drawTo x | isInfixOf "A" x = "A"
drawTo x | isInfixOf "B" x = "B"
drawTo x | isInfixOf "C" x = "C"

winTo :: String -> String
winTo x | isInfixOf "A" x = "B"
winTo x | isInfixOf "B" x = "C"
winTo x | isInfixOf "C" x = "A"

score :: String -> Int
score x | x `elem` ["A X", "B X", "C X"] = 0 + (points $ loseTo x)
score x | x `elem` ["A Y", "B Y", "C Y"] = 3 + (points $ drawTo x)
score x | x `elem` ["A Z", "B Z", "C Z"] = 6 + (points $ winTo x)
score _ = error "Invalid input"

solution :: String -> String
solution s = show $ sum $ map score $ lines s

main :: IO ()
main = interact solution
