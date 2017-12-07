import qualified Data.Map.Strict as M

-- Puzzle 1: manhattan distance from input to 1
-- on the spiral grid
--
-- 17  16  15  14  13
-- 18   5   4   3  12
-- 19   6   1   2  11
-- 20   7   8   9  10
-- 21  22  23---> ...
--
-- for i = 1 .. infinity step 2:
--    right i
--    up i
--    left i+1
--    down i+1
--
--    above 1: 3..5, 12..18, 29..39,
--    below 1: 7..9, 20..27, 41..51
--
data Dir = GoUp | GoLeft | GoDown | GoRight deriving Show

segment :: Int -> [Dir]
segment 0 = []
segment i = replicate i GoRight ++ replicate i GoUp ++ replicate (i+1) GoLeft ++ replicate (i+1) GoDown

grid :: [Dir]
grid = [1,3..] >>= segment


computePositions :: [(Int, Int)]
computePositions = scanl update (0,0) grid

puzzle1 n = let (x, y) = computePositions !! ((read n) - 1)
            in abs x + abs y
 

update :: (Int, Int) -> Dir -> (Int, Int)
update (x, y) = \d -> case d of 
                         GoLeft -> (x - 1, y)
                         GoRight -> (x + 1, y)
                         GoUp -> (x, y + 1)
                         GoDown -> (x, y - 1)

-- Puzzle 2 - fill in order each square with the sum of adjacent squares
--            unvisited squares are zeroed
type Grid = M.Map (Int, Int) Integer

neighbors (x,y) = (,) <$> [x-1,x,x+1] <*> [y-1,y,y+1]

addPosition :: Grid -> (Int, Int) -> Grid
addPosition g square = M.insert square value g
                       where value = sum [M.findWithDefault 0 p g | p <- neighbors square]

filledGrid :: [Grid]
filledGrid = scanl addPosition (M.singleton (0,0) 1) (drop 1 computePositions)

-- Trial and error with
-- (\n -> (filledGrid !! n) M.! (computePositions !! n))
-- showed that position 59 got the value 295229
-- Should still write a search function that returns it for me




--stressTest = scanl addPosition (M.singleton (0,0) 1) computePositions
puzzle2 = id

main = do 
     puzzle1 <$> readFile "day3.input"  >>= print  -- 419
     puzzle2 <$> readFile "day3.input"  >>= print  -- ???
