import qualified Data.Set as S
import Control.Arrow

isValidPassphrase :: [String] -> Bool
isValidPassphrase pp = length pp == length (S.fromList pp)

puzzle1 :: String -> Int
puzzle1 = length . filter (isValidPassphrase . words) . lines
               
puzzle2 = const 0

main = do 
     puzzle1 <$> readFile "day4.input"  >>= print -- 451
     puzzle2 <$> readFile "day4.input"  >>= print
