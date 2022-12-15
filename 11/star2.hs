import Data.Text (splitOn, pack, unpack)
import Data.List (isSuffixOf, intercalate, sort)

data Monkey = Monkey {
  items :: [Int],
  op :: Expr,
  check :: Int,
  left_dest :: Int,
  right_dest :: Int,
  inspect_count :: Int
} deriving (Show)

data Expr = Old | Val Int | Operator Expr String Expr deriving (Show)

eval :: Expr -> Int -> Int
eval (Operator lhs "+" rhs) old = (eval lhs old) + (eval rhs old)
eval (Operator lhs "*" rhs) old = (eval lhs old) * (eval rhs old)
eval (Val x) old = x
eval Old old = old

parse_expr :: String -> Expr
parse_expr s = expr
  where (lhs:op:rhs:[]) = words s
        l = if lhs == "old" then Old else Val (read lhs)
        r = if rhs == "old" then Old else Val (read rhs)
        expr = (Operator l op r)


parse_starting_items :: String -> [Int]
parse_starting_items s = map (read . dropComma) string_items
  where string_items = tail $ dropWhile (not . (":" `isSuffixOf`)) (words s)
        dropComma [] = []
        dropComma (',':[]) = []
        dropComma (a:as) = a : (dropComma as)


parse_op :: String -> Expr
parse_op s = parse_expr $ intercalate " " $ reverse $ takeWhile (/= "=") (reverse $ words s)

parse_last_word :: String -> Int
parse_last_word = read . last . words

parse_monkey :: String -> Monkey
parse_monkey s = Monkey {
  items = parse_starting_items (ls!!1),
  op = parse_op (ls!!2),
  check = parse_last_word (ls!!3),
  left_dest = parse_last_word (ls!!5),
  right_dest = parse_last_word (ls!!4),
  inspect_count = 0
}
  where ls = lines s

parse_monkeys :: String -> [Monkey]
parse_monkeys s = map parse_monkey raw
  where raw = map unpack $ splitOn (pack "\n\n") (pack s)

update :: [a] -> Int -> (a -> a) -> [a]
update [] index updater = []
update (x:xs) 0 updater = (updater x):xs
update (x:xs) n updater = x : update xs (n - 1) updater

-- Actual Round Logic
throw_item :: Int -> [Monkey] -> Int -> [Monkey]
throw_item index monkeys value = update monkeys index (\(m@Monkey { items = is }) -> m { items = is ++ [value] })

process_item :: Int -> Monkey -> Int -> [Monkey] -> [Monkey]
process_item outerMod Monkey { op = expr, check = divisor, left_dest = l, right_dest = r } value monkeys = updated_monkeys
  where new_value = (eval expr value) `mod` outerMod
        dest = if new_value `mod` divisor == 0 then r else l
        updated_monkeys = throw_item dest monkeys new_value


process_monkey :: Int -> [Monkey] -> Int -> [Monkey]
process_monkey outerMod monkeys i = (update updated_monkeys i (\_ -> new_monkey))
  where m = monkeys!!i
        count = inspect_count m
        new_monkey = m { items = [], inspect_count = count + (length $ items m) }
        updated_monkeys = foldl (flip (process_item outerMod m)) monkeys (items m)

loop :: (a -> a) -> Int -> a -> a
loop thunk 0 initial = initial
loop thunk n initial = loop thunk (n - 1) (thunk initial)

perform_round :: Int -> [Monkey] -> [Monkey]
perform_round outerMod ms = foldl (process_monkey outerMod) ms [0..((length ms) - 1)]

calc_monkey_business :: [Monkey] -> Int
calc_monkey_business = product . (take 2) . reverse . sort . map inspect_count

solution :: String -> String
solution input = (show $ calc_monkey_business $ loop (perform_round outerMod) 10000 $ monkeys) ++ "\n"
  where monkeys = parse_monkeys input
        outerMod = product $ (map check) monkeys

main :: IO ()
main = interact solution
