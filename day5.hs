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
          

-- Given a program, run the program and return
-- the number of steps taken.
runProgram :: Program -> Int
runProgram p = fst $ execState (run' 0) (0, p)
   where run' i = do
             new <- step i
             --trace (show new) (return ())
             if new >= length p then get else run' new
               


puzzle1 = runProgram . program
puzzle2 = const 0

main = do 
     puzzle1 <$> readFile "day5.input"  >>= print -- 356945
     puzzle2 <$> readFile "day5.input"  >>= print
