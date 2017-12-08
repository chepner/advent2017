
puzzle1 s = 0
puzzle2 s = 0

main = do 
       s <- readFile "day7.input"
       print (puzzle1 s)
       print (puzzle2 s)
