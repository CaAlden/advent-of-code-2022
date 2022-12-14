import Control.Monad
import Data.List (intercalate, sort)
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

compare_container :: Ord a => Container a -> Container a -> Ordering
compare_container Nested { items = [] } Nested { items = [] } = EQ
compare_container _ Nested { items = [] } = GT
compare_container Nested { items = [] } _ = LT
compare_container (Value a) (Value b) | a == b = EQ
compare_container (Value a) (Value b) | a < b = LT
compare_container (Value a) (Value b) = GT

compare_container (Value a) (b@Nested {}) = compare_container Nested { items = [(Value a)] } b
compare_container (a@Nested {}) (Value b) = compare_container a Nested { items = [(Value b)] }
compare_container Nested { items = (x:xs) } Nested { items = (y:ys) } =
  if r == EQ
     then compare_container Nested { items = xs } Nested { items = ys }
     else r
       where r = compare_container x y

instance (Ord a) => Ord (Container a) where
  a `compare` b = (compare_container a b)

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

parse_input :: String -> [Container Int]
parse_input s = reduced
  where input = join $ map lines $ map unpack $ splitOn (pack "\n\n") (pack s)
        containers = map parse_container input
        ex Nested { items = i } = head i
        reduced = map ex containers

-- Logic

-- IO

s_divider = Nested { items = [Nested { items = [(Value 2)] }] }
e_divider = Nested { items = [Nested { items = [(Value 6)] }] }

solution :: String -> String
solution s = (show answer) ++ "\n"
  where input = parse_input s ++ [s_divider, e_divider]
        sorted = sort input
        indexed = zip ([1..]) sorted
        filtered = filter ((\x -> x == s_divider || x == e_divider) . snd) indexed
        answer = product $ map fst filtered

main :: IO ()
main = interact solution
