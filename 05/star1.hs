import Data.Text (unpack, pack, splitOn)
import Data.List (partition, transpose, intercalate)

data Action = Action {
  source :: Int,
  destination :: Int,
  amount :: Int
} deriving (Show)

parse_action :: String -> Action
parse_action a = Action {
  amount = read (vals!!0),
  source = read (vals!!1),
  destination = read (vals!!2)
}
  where vals = map snd $ fst $ (partition (even . fst)) (zip [1..] (words a))

parse_actions :: String -> [Action]
parse_actions = (map parse_action) . lines

parse_crates :: String -> String
parse_crates c = map snd $ fst $ partition ((\x -> (x - 2) `mod` 4 == 0) . fst) (zip [1..] c)

parse_all_crates :: String -> [String]
parse_all_crates input = map (filter (\x -> x /= ' ')) $ spaced
  where spaced = transpose $ init $ (map parse_crates) $ (lines input)

set :: Int -> a -> [a] -> [a]
set index value list = left ++ value : rest
  where (left, _:rest) = splitAt index list

move :: Int -> Int -> [[a]] -> [[a]]
move f t l = set to dest (set from src l)
  where from = f - 1
        to = t - 1
        src = tail (l!!from)
        dest = (head (l!!from)) : l!!to

apply_action :: Action -> [[a]] -> [[a]]
apply_action Action { amount = 0 } l = l
apply_action Action { amount = a, source = s, destination = d } l =
  apply_action Action { amount = a - 1, source = s, destination = d } updated
    where updated = move s d l

apply_actions :: [Action] -> [[a]] -> [[a]]
apply_actions [] list = list
apply_actions (a:as) list = apply_actions as $ apply_action a list

list2Tup :: [a] -> (a, a)
list2Tup (a:b:[]) = (a, b)


solution :: String -> String
solution i = show $ map head $ apply_actions (parse_actions $ unpack moves) $ (parse_all_crates $ unpack crates)
  where (crates, moves) = list2Tup $ splitOn (pack "\n\n") (pack i)

main :: IO ()
main = interact solution
