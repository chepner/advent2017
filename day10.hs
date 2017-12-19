module Day10 where

import qualified Text.Parsec as T
import Control.Monad.Trans.State
import Control.Monad
import Debug.Trace

type List = ( [Int]  -- State of the circular list
            , Int    -- current position
            , Int    -- current step size
            )

initialState = ([0..255], 0, 0)

step :: Int -> State List ()
step inpLength = do
   (list, cp, ss) <- get
   let n = length list
       sublist = take inpLength . drop cp $ list ++ list
       revSublist = reverse sublist
       m = n - (cp + inpLength)
       newList = if m >= 0
                  then (take cp list) ++ revSublist ++ (drop (cp + inpLength) list)
                  else let extra = cp + inpLength - n
                       in drop (inpLength - extra) revSublist ++
                          take (cp - extra) (drop extra list) ++
                          take (inpLength - extra) revSublist
       newState = (newList, (cp + inpLength + ss) `mod` n, ss + 1)
   put (trace (show newState) newState)

puzzle1 :: [Int] -> State List ()
puzzle1 = foldM (const step) ()

parseInput = T.parse inputP ""
    where inputP = T.sepBy (T.many T.digit) (T.char ',')

main = do
   s <- readFile "day10.input"
   let Right inputs = fmap (map read) $ parseInput s
   let ((x:y:_),_,_) = execState (puzzle1 inputs) initialState
   print $ x * y  -- Not 506 or 72. I was going to be surprised if that were right
    -- 34717 is too low...
    -- 38415!
   
