import Data.List
import qualified Data.Set as S
import Control.Arrow

isValidPassphrase :: [String] -> Bool
isValidPassphrase pp = length pp == length (S.fromList pp)

isValidPassphrase2 :: [String] -> Bool
isValidPassphrase2 pp = length pp == length (S.fromList (map sort pp))

puzzle1 :: String -> Int
puzzle1 = length . filter (isValidPassphrase . words) . lines
               
puzzle2 :: String -> Int
puzzle2 = length . filter (isValidPassphrase2 . words) . lines

main = do 
     puzzle1 <$> readFile "day4.input"  >>= print -- 451
     puzzle2 <$> readFile "day4.input"  >>= print -- 223
