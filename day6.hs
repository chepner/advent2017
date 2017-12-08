import Data.Monoid
import Control.Monad.Trans.State
import Data.Array as A
import Data.Function
import Data.List
import qualified Data.Set as S

{-
 - We'll need a type to represent memory, a function
 - to perform the mancala-like balancer, and a state
 - to remember which allocations have been seen.
 - The answer to puzzle 1 is the size of the
 - set when we encounter the first duplicate allocation
 -}

type Address = Int
type BankSize = Int
type Memory = A.Array Address BankSize

createMemory :: [BankSize] -> Memory
createMemory xs = A.listArray (0, length xs - 1) xs

bankCompare :: (Address, BankSize) -> (Address, BankSize) -> Ordering
-- Largest size, with ties broken by *smallest* address
bankCompare (a1, s1) (a2, s2) = compare s1 s2 <> compare a2 a1

nextBank :: Memory -> Address -> Address
-- cyclic successor of an Address
nextBank m b = let n = snd . bounds $ m 
               in if b == n then 0 else b + 1

updateBank :: Memory -> Address -> Memory
updateBank m a = m A.// [(a, (m A.! a) + 1)]

distribute :: Memory -> Address -> BankSize -> Memory
distribute m _ 0 = m
distribute m start n = distribute m' start' (n - 1)
                       where m' = updateBank m start
                             start' = nextBank m start

mancala :: Memory -> Memory
mancala m = let (start, amount) = maximumBy bankCompare . assocs $ m
                pickedUp = m A.// [(start, 0)]
            in distribute pickedUp (nextBank m start) amount

run :: Memory -> State (S.Set Memory) Int
run m = do
         seen <- get
         let m' = mancala m
         if m' `S.member` seen
          then return (S.size seen)
          else put (S.insert m' seen) >> run m'


-- How long until we hit an infinite loop?
puzzle1 s = let sizes = read <$> words s
                m = createMemory sizes
            in evalState (run m) (S.singleton m)

--  How many cycles are in the infinite loop?
--  Number each set as it is added, but set equality
--  is based on on the set, not its label

run2 :: Memory -> Int -> State [(Memory, Int)] Int
run2 m curr = do
         seen <- get
         let m' = mancala m
         case lookup m' seen of
          Just n -> return (curr - n)
          Nothing -> put ((m', curr):seen) >> run2 m' (curr + 1)


-- How long until we hit an infinite loop?
puzzle2 s = let sizes = read <$> words s
                m = createMemory sizes
            in evalState (run2 m 0) [(m,0)]

main = do 
       s <- readFile "day6.input"
       print (puzzle1 s) -- 11137
       print (puzzle2 s) -- 1037
