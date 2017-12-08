import qualified Data.Array as A
import Debug.Trace
--import Data.List.Lens
--import Control.Lens
import Control.Monad.Trans.State

{-
 - Start with a list of integers
 - 
 - Read instruction: jump, then increment destination
 -}

type Count = Int
type StepSize = Int
type Program = A.Array Int StepSize

program :: String -> Program
program s = let instructions = [(i, read x) | (i,x) <- zip [0..] (lines s)]
            in A.array (0,length instructions - 1) instructions

step :: StepSize 
     -> State (Count, Program) Int -- New position with incremented program
step n = do
          (count, program) <- get
          let offset = program A.! n
              new_position = n + offset
          put (count + 1, program A.// [(n, offset + 1)])
          return new_position

step2 :: StepSize 
     -> State (Count, Program) Int -- New position with incremented program
step2 n = do
          (count, program) <- get
          let offset = program A.! n
              new_position = n + offset
              modify val | val >= 3 = val - 1
                         | otherwise = val + 1
          put (count + 1, program A.// [(n, modify offset)])
          return new_position
          

-- Given a program, run the program and return
-- the number of steps taken.
runProgram :: (StepSize -> State (Count, Program) Int) -> Program -> Int
runProgram step p = fst $ execState (run' 0) (0, p)
   where run' i = do
             new <- step i
             if new >= length p then get else run' new
               


puzzle1 = runProgram step . program
puzzle2 = runProgram step2 . program

main = do 
       s <- readFile "day5.input"
       print (puzzle1 s) --   356945
       print (puzzle2 s) -- 28372145
