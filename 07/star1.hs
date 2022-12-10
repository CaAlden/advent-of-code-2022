import Control.Monad (join)
import Data.Foldable
import Data.Monoid
import Data.List (intercalate)

data FileTree =
  File { size :: Int, name :: String } | Dir { entries :: [FileTree], name :: String }
  deriving (Show)

is_dir :: FileTree -> Bool
is_dir Dir {} = True
is_dir _ = False

fold_file_tree :: (a -> FileTree -> a) -> FileTree -> a -> a
fold_file_tree f dir@Dir { entries = e } base = foldl (\b file -> (fold_file_tree f file b)) (f base dir) e
fold_file_tree f file base = f base file

insert :: FileTree -> FileTree -> FileTree
insert value Dir { entries = e, name = n } = Dir { entries = value:e, name = n }
insert value _ = value

-- Inserting at "/" => insert_at [] File {...} Dir { name = "/" ... }
-- Inserting at a sub directory => insert_at ["a", "b", "c"] File { ... } Dir { name = "/" ... }
insert_at :: [String] -> FileTree -> FileTree -> FileTree
insert_at [] value tree = insert value tree
insert_at (x:xs) value Dir { entries = e, name = n } = Dir {
  name = n,
  entries = insertedChild
}
  where insertedChild = map insertOrSkip $ e
        insertOrSkip sub@(Dir { name = sn }) | sn == x = insert_at xs value sub
        insertOrSkip a = a

update_filetree :: FileTree -> [String] -> String -> ([String], FileTree)
update_filetree tree cursor "$ cd /" = ([], tree)
update_filetree tree cursor "$ cd .." = (tail cursor, tree)
update_filetree tree cursor ('$':' ':'c':'d':' ':dir) = (dir:cursor, tree)
update_filetree tree cursor "$ ls" = (cursor, tree)
update_filetree tree cursor ('d':'i':'r':' ':n) = (cursor, insert_at (reverse cursor) Dir { name = n, entries = [] } tree)
update_filetree tree cursor file = (cursor, insert_at (reverse cursor) File { name = n, size = read s } tree)
  where (s:n:[]) = words file

apply_commands :: [String] -> FileTree -> FileTree
apply_commands commands tree = go commands tree []
  where go [] tree _ = tree
        go (c:cs) tree ctx = go cs new_tree new_context
          where (new_context, new_tree) = update_filetree tree ctx c

parse_directory_structure :: String -> FileTree
parse_directory_structure i = apply_commands (lines i) Dir { name = "/", entries = [] }

calculate_size :: FileTree -> Int
calculate_size Dir { entries = e } = sum $ map calculate_size e
calculate_size File { size = s } = s


pretty_print :: FileTree -> String
pretty_print tree = go 0 tree
  where go depth Dir { name = n, entries = e } = (foldr (++) [] (replicate depth " ")) ++ "- " ++ n ++ " (dir)\n" ++ (intercalate "\n" $ map (go (depth + 2)) e)
        go depth File { name = n, size = i } = (foldr (++) [] (replicate depth " ")) ++ "- " ++ n ++ " (file, size=" ++ (show i) ++ ")"

solution :: String -> String
solution i = show $
  sum $
  map calculate_size $
  filter (\x -> (is_dir x) && (calculate_size x) <= 100000) $
  fold_file_tree (\acc ft -> ft:acc) (parse_directory_structure i) []

main :: IO ()
main = interact solution
