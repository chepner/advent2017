import Control.Arrow

minMaxDiff :: [Int] -> Int
-- minMaxDiff xs = (maximum xs) - (minimum xs)
minMaxDiff = uncurry (-) . (maximum &&& minimum)

parseSpreadsheet :: String -> [[Int]]
parseSpreadsheet s = fmap (fmap read) $ (fmap words <$> lines $ s)

puzzle1 :: String -> Int
puzzle1 = sum . fmap minMaxDiff . parseSpreadsheet

-- Puzzle 2: replace minMaxDiff with something that returns div n d for
-- the only two numbers such that mod n d == 0
--

getQuotient :: [Int] -> [Int]
getQuotient xs = [n `div` d | (n, d) <- (,) <$> xs <*> xs, n > d, n `mod` d == 0]


puzzle2 :: String -> Int
puzzle2 = sum . concat . map getQuotient . parseSpreadsheet



-- Answer should be 53460, 282
main = do
         puzzle1 <$> readFile "day2.input" >>= print
         puzzle2 <$> readFile "day2.input" >>= print
