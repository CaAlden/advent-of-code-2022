-- Data Definitions
import Data.Text (splitOn, pack, unpack)
import Data.List (replicate)
import Data.Maybe

type Point = (Int, Int, Int)
type Edge = Bool
data Cube = Cube {
  posn :: Point,
  -- [up, down, right, left, front, back]
  edges :: [Edge]
} deriving (Show)

explodeEdgePoints :: Cube -> [Point]
explodeEdgePoints Cube { posn = (x, y, z) } = [
  (x, y, z + 1),
  (x, y, z - 1),
  (x + 1, y, z),
  (x - 1, y, z),
  (x, y + 1, z),
  (x, y - 1, z)]

makeCube :: Point -> Cube
makeCube pos = Cube {
  posn = pos,
  edges = replicate 6 False
}

parsePoint :: String -> Point
parsePoint input = (w!!0, w!!1, w!!2)
  where w = (map (read . unpack)) $ splitOn (pack ",") (pack input)

markInside :: [Cube] -> [Point] -> [Cube]
markInside [] _ = []
markInside (c:rest) points = updated : (markInside rest points)
  where up:down:right:left:front:back:[] = explodeEdgePoints c
        cUp:cDown:cRight:cLeft:cFront:cBack:[] = edges c
        updated = c { edges = [
          cUp || up `elem` points,
          cDown || down `elem` points,
          cRight || right `elem` points,
          cLeft || left `elem` points,
          cFront || front `elem` points,
          cBack || back `elem` points
        ]}

countOutsideEdges :: [Cube] -> Int
countOutsideEdges = foldr (\a s -> s + (length $ filter (not) $ edges a)) 0

solution :: String -> String
solution input = show $ countOutsideEdges marked
  where points = map parsePoint $ lines input
        cubes = map makeCube points
        marked = markInside cubes points

main :: IO ()
main = interact solution
