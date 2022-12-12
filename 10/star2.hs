data State = State {
  x :: Int,
  clock :: Int
} deriving (Show)

data Instruction = Instruction {
  cycles :: Int,
  name :: String
} deriving (Show)

eval :: String -> State -> State
eval "noop" State { x = currentX, clock = clk } = State { x = currentX, clock = clk + 1 }
eval ('a':'d':'d':'x':' ':args) State { x = currentX, clock = clk } = State { x = currentX + (read args), clock = clk + 2 }

inc :: State -> Int -> [State]
inc s 1 = [s]
inc (s@State { clock = c, x = i }) n = s : inc newState (n - 1)
  where newState = State { clock = c + 1, x = i }

fillOut :: [State] -> [State]
fillOut (s:s2:[]) = (inc s ((clock s2) - (clock s))) ++ [s2]
fillOut (s:s2:rest) = (inc s ((clock s2) - (clock s))) ++ fillOut (s2:rest)

dist :: Num a => a -> a -> a
dist a b = abs (b - a)

drawCrt :: [State] -> String
drawCrt states = go "\n" states
  where go acc s | ((length acc) `mod` 41) == 0 = go (acc ++ "\n") s
        go acc [] = acc ++ "\n"
        go acc (s:rest) = go (acc ++ [ch]) rest
          where crtPos = ((clock s) `mod` 40) - 1
                currentX = x s
                ch = if (dist crtPos currentX) > 1 then '.' else '#'


solution :: String -> String
solution i = drawCrt full
  where instructions = lines i
        sparse = (scanl (flip eval) State { x = 1, clock = 1 } instructions)
        full = init $ fillOut sparse

main :: IO ()
main = interact solution
