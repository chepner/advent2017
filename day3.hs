-- Puzzle 1: manhattan distance from input to 1
-- on the spiral grid
--
-- 17  16  15  14  13
-- 18   5   4   3  12
-- 19   6   1   2  11
-- 20   7   8   9  10
-- 21  22  23---> ...

puzzle1 = id
puzzle2 = id

main = do 
     puzzle1 <$> readFile "day3.input"  >>= print
     puzzle2 <$> readFile "day3.input"  >>= print
