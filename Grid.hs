module Grid (
  Grid,
  rowCount,
  columnCount,
  fromListWithDims,
  fromLists,
  generateGrid,
  replace,
  valueAt
) where

import Data.List (replicate, intercalate)
import Control.Monad (join)

data Grid a = Grid {
  items :: [[a]],
  columnCount :: Int,
  rowCount :: Int
}

pad :: Int -> String -> String
pad 0 s = s
pad n _ | n < 0 = error "Cannot pad with a negative number: " ++ show n
pad n [] = ' ' : pad (n - 1) []
pad n (x:xs) = x : pad (n - 1) xs

instance (Show a) => Show (Grid a) where
  show Grid { items = is, columnCount = c } = contents
    where stringified = map (map show) is
          longest = maximum $ map length $ join stringified
          padded = map (map (pad longest)) stringified
          contents = intercalate "\n" $ (map (intercalate " ") padded)

instance Functor Grid where
  fmap f g@Grid { items = i } = g { items = fmap (fmap f) i }

instance Foldable Grid where
  foldMap f g@Grid { items = i } = foldMap (foldMap f) i

fromListWithDims :: (Int, Int) -> [a] -> Grid a
fromListWithDims (r, c) list = Grid {
  rowCount = r,
  columnCount = c,
  items = groupUp r c list
}
  where groupUp 0 0 _ = []
        groupUp y x l = [take x l] ++ groupUp (y - 1) (x - 1) (drop x l)

fromLists :: [[a]] -> Grid a
fromLists is = Grid {
  items = is,
  rowCount = length is,
  columnCount = length (is!!0)
}

generateGrid :: (Int, Int) -> a -> Grid a
generateGrid (rs, cs) i = Grid {
  rowCount = rs,
  columnCount = cs,
  items = (replicate rs (replicate cs i))
}

setListValue :: a -> Int -> [a] -> [a]
setListValue v 0 (x:xs) = v : xs
setListValue v n _ | n < 0 = error "index was negative"
setListValue v n [] | n > 0 = error "index was out of bounds"
setListValue v n (x:xs) = x : (setListValue v (n - 1) xs)

replace :: (Int, Int) -> a -> Grid a -> Grid a
replace (r, c) val grid@Grid { items = i } = grid {
  items = setListValue updatedRow r i
}
  where updatedRow = setListValue val c (i!!r)

valueAt :: (Int, Int) -> Grid a -> a
valueAt (row, col) Grid { items = i} = (i!!row)!!col
