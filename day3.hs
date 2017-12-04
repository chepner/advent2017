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

-- Trial-and-error shows that 289444 is the upper-left corner 
-- puzzle1 n' = let n = read n' in take 320 $ drop (n - 160) (zip [1..] grid)
puzzle1 n = let (x, y) = scanl update (0,0) grid !! ((read n) - 1)
            in abs x + abs y
 

update :: (Int, Int) -> Dir -> (Int, Int)
update (x, y) = \d -> case d of 
                         GoLeft -> (x - 1, y)
                         GoRight -> (x + 1, y)
                         GoUp -> (x, y + 1)
                         GoDown -> (x, y - 1)

puzzle2 = id

main = do 
     puzzle1 <$> readFile "day3.input"  >>= print
     puzzle2 <$> readFile "day3.input"  >>= print
