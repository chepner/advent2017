import Control.Arrow

minMaxDiff :: [Int] -> Int
minMaxDiff = uncurry (-) . (maximum &&& minimum)

parseSpreadsheet :: String -> [[Int]]
parseSpreadsheet s = fmap (fmap read) $ (fmap words <$> lines $ s)



-- Answer should be 53460
main = (sum . fmap minMaxDiff . parseSpreadsheet) <$> readFile "day2.input" >>= print
