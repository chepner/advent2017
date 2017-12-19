module Day10 where

import qualified Text.Parsec as T
import Control.Monad.Trans.State
import Control.Monad
import Debug.Trace
import Data.Char
import Data.List
import Data.List.Split
import Data.Bits
import Text.Printf

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
   put newState

puzzle :: [Int] -> State List ()
puzzle = foldM (const step) ()

parseInput = (++ [17,31,73,47,23]) . map ord

main = do
   s <- readFile "day10.input"
   let inputs = concat . replicate 64 . parseInput $ s
       (sparseHash, _, _) = execState (puzzle inputs) initialState
       denseHash = map (foldl1' xor) . chunksOf 16 $ sparseHash
   putStrLn $ denseHash >>= printf "%02x"
-- Not c498762dc2a7a03f10672138ac31dde
-- Not c498762dc2a7a03f100672138ac31dde either
   
