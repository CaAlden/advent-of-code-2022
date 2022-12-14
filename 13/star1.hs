import Control.Monad
import Data.List (intercalate)
import Data.Text (splitOn, pack, unpack)

-- Data Definitions

data Container a = Nested {
  items :: [Container a]
} | Value (a)

instance (Show a) => Show (Container a) where
  show Nested { items = i } = "<" ++ (intercalate "," $ map show i) ++ ">"
  show (Value x) = "*" ++ show x ++ "*"

unwrap :: Container (Container a) -> [Container a]
unwrap Nested { items = i } = join $ fmap unwrap i
unwrap (Value x) = [x]

instance Monad Container where
  (>>=) (Value x) f = f x
  (>>=) Nested { items = i } f = Nested { items = join $ map (unwrap . (fmap f)) i }
  (>>) a b = b
  return a = Value a

instance Functor Container where
  fmap = liftM

instance Applicative Container where
  pure = return
  (<*>) = ap

instance (Eq a) => Eq (Container a) where
  Nested { items = a } == Nested { items = b} = a == b
  (Value a) == (Value b) = a == b
  _ == _ = False

-- Parsing

bracket_span :: String -> Int -> String
bracket_span (']':rest) 0 = []
bracket_span (']':rest) n = ']' : (bracket_span rest (n - 1))
bracket_span ('[':rest) n = '[' : (bracket_span rest (n + 1))
bracket_span (x:xs) n = x : bracket_span xs n

parse_ctx :: Container String -> String -> String -> Container String
parse_ctx container ("") ("") = container
parse_ctx Nested { items = i } s "" = Nested { items = i ++ [(Value s)] }
parse_ctx container _ ('[':inner) = parse_ctx next "" (drop ((length inner_bracket_span) + 1) inner)
  where inner_bracket_span = bracket_span inner 0
        next = Nested { items = (items container) ++ [parse_ctx Nested { items = [] } "" inner_bracket_span] }

parse_ctx container@Nested { items = i } current (',':rest) =
  parse_ctx Nested { items = i ++ [(Value current)] } "" rest
parse_ctx container current (x:xs) = parse_ctx container (current ++ [x]) xs

string_to_int_container :: Container String -> Container Int
string_to_int_container Nested { items = i } = Nested { items = map (string_to_int_container) filtered_items }
  where filtered_items = filter (\s -> s /= (Value "")) i
string_to_int_container (Value x) = (Value (read x))

parse_container :: String -> Container Int
parse_container = string_to_int_container . (parse_ctx Nested { items = [] } "")

parse_groups :: [String] -> [(Container Int, Container Int)]
parse_groups [] = []
parse_groups (a:b:rest) = (parse_container a, parse_container b) : parse_groups rest

parse_input :: String -> [(Container Int, Container Int)]
parse_input s = reduced
  where groups = parse_groups $ join $ map lines $ map unpack $ splitOn (pack "\n\n") (pack s)
        ex Nested { items = i } = head i
        reduced = map (\(a, b) -> (ex a, ex b)) groups



-- Logic

data Decision = Good | Bad | Unknown deriving (Show, Eq)

compare_container :: Ord a => Container a -> Container a -> Decision
compare_container Nested { items = [] } Nested { items = [] } = Unknown
compare_container _ Nested { items = [] } = Bad
compare_container Nested { items = [] } _ = Good
compare_container (Value a) (Value b) | a == b = Unknown
compare_container (Value a) (Value b) | a < b = Good
compare_container (Value a) (Value b) = Bad

compare_container (Value a) (b@Nested {}) = compare_container Nested { items = [(Value a)] } b
compare_container (a@Nested {}) (Value b) = compare_container a Nested { items = [(Value b)] }
compare_container Nested { items = (x:xs) } Nested { items = (y:ys) } =
  if r == Unknown
     then compare_container Nested { items = xs } Nested { items = ys }
     else r
       where r = compare_container x y

-- IO

solution :: String -> String
solution s = (show solved)  ++ "\n"
  where decisions = map (uncurry compare_container) $ parse_input s
        indexed = zip [1..] decisions
        filtered = filter ((== Good) . snd) indexed
        solved = sum $ map fst filtered

main :: IO ()
main = interact solution
