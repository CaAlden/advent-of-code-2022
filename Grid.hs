module Grid (
  Grid,
  rowCount,
  columnCount,
  fromListWithDims,
  fromLists,
  generateGrid,
  replace,
  valueAt,
  valueAtSafe,
  coords,
  findInGrid,
  findIndexInGrid,
  findLastIndexWhere,
  growGridRows
) where

import Data.List (replicate, intercalate)
import Data.Maybe
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
setListValue v n [] | n >= 0 = error "index was out of bounds"
setListValue v n (x:xs) = x : (setListValue v (n - 1) xs)

replace :: (Int, Int) -> a -> Grid a -> Grid a
replace (r, c) val grid@Grid { items = i } = grid {
  items = setListValue updatedRow r i
}
  where updatedRow = setListValue val c (i!!r)

valueAtSafe :: (Int, Int) -> Grid a -> Maybe a
valueAtSafe (row, col) Grid { rowCount = r, columnCount = c } | row < 0 || row >= r || col < 0 || col >= c = Nothing
valueAtSafe (row, col) Grid { items = i } = Just ((i!!row)!!col)

valueAt :: (Int, Int) -> Grid a -> a
valueAt p g = unwrap $ valueAtSafe p g
  where unwrap (Just v) = v
        unwrap Nothing = error ("no value at " ++ (show p))

coords :: Grid a -> [(Int, Int)]
coords g = [(r, c) | r <- [0..(rowCount g) - 1], c <- [0..(columnCount g) - 1] ]

findInGrid :: (a -> Bool) -> Grid a -> Maybe a
findInGrid f g = foldr match Nothing g
  where match _ (Just x) = Just x
        match val Nothing | f val = Just val
        match _ _ = Nothing

findIndexInGrid :: (a -> Bool) -> Grid a -> Maybe (Int, Int)
findIndexInGrid pred g = foldr match Nothing (coords g)
  where match _ (Just coord) = Just coord
        match p Nothing | pred (valueAt p g) = Just p
        match _ _ = Nothing

findLastIndexWhere :: (a -> Bool) -> Grid a -> Maybe (Int, Int)
findLastIndexWhere pred grid = foldl match Nothing (coords grid)
  where match acc p = if pred (valueAt p grid) then Just p else acc

growGridRows :: Int -> a -> Grid a -> Grid a
growGridRows n a Grid { rowCount = r, columnCount = c, items = i} = Grid {
  rowCount = r + n,
  columnCount = c,
  items = i ++ (items $ generateGrid (n, c) a)
}

