puzzle1 = const 0
puzzle2 = const 0

main = do 
     puzzle1 <$> readFile "day5.input"  >>= print
     puzzle2 <$> readFile "day5.input"  >>= print
