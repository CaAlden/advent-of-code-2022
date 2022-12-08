import Control.Monad (join)
import Control.Arrow ((***))
{-
  1. Parse the ranges
-}

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

data Range = Range Int Int deriving (Show)

split :: Char -> String -> (String, String)
split on (f:rest) | on == f = ([], rest)
split on (f:rest) = (f : (fst rec), snd rec)
  where rec = split on rest

parse_range :: String -> Range
parse_range s = Range (read $ fst sides) (read $ snd sides)
  where sides = split '-' s

parse_input :: String -> (Range, Range)
parse_input s = mapTuple parse_range $ split ',' s

overlaps :: Range -> Range -> Bool
overlaps (Range s1 e1) (Range s2 e2) =
  (s2 <= e1 && e2 >= e1) || (s1 <= e2 && e1 >= e2)

solution :: String -> String
solution input = show $ length $ filter (uncurry overlaps) $ map parse_input $ lines input

main :: IO ()
main = interact solution
