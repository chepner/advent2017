module Day13 where

import qualified Text.Parsec as T
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)
import Data.List.Split
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Reader
import Debug.Trace

lexer = Tok.makeTokenParser emptyDef


parseLine :: String -> Either T.ParseError FirewallLayer
parseLine = let p = do depth <- Tok.integer lexer
                       Tok.symbol lexer ": "
                       range <- Tok.integer lexer
                       return (depth, range)
            in T.parse p ""
                   

type Depth = Integer
type Range = Integer
type Time = Integer
type Delay = Integer
type Severity = Integer
type FirewallLayer = (Depth, Range)
type Firewall = M.Map Depth Range

severity :: Time -> Reader Firewall Severity
severity t = do
   fw <- ask
   let period r = 2 * r - 2
   return $ case M.lookup t fw of
             Nothing -> 0 -- No scanner at this depth
             Just r ->  if t `mod` (period r) == 0 then r * t else 0

severity2 :: Delay -> Time -> Reader Firewall Severity
severity2 d t = do
   fw <- ask
   let period r = 2 * r - 2
       depth = t - d
   return $ case M.lookup depth fw of
             Nothing -> 0 -- No scanner at this depth
             Just r ->  if t `mod` (period r) == 0 then r * depth else 0

run :: Reader Firewall Severity
run = do
    fw <- ask
    let s = maximum $ M.keys fw 
    severities <- traverse severity [0..s]
    return $ sum severities

runWithDelay :: Delay -> Reader Firewall Severity
runWithDelay d = do
    fw <- ask
    let s = maximum $ M.keys fw 
        delayedTimes = map (+d) [0..s]
    severities <- traverse (severity2 d) delayedTimes
    return $ sum severities

findMinDelay :: Reader Firewall Delay
findMinDelay = do
    fw <- ask
    let foo' d = let s = runReader (runWithDelay d) fw
                     msg = "Severity at " ++ show d ++ " is " ++ show s
                 in if (trace msg s) == 0 then return d else foo' (d + 1)
    foo' 0

main = do
  s <- lines <$> readFile "day13.sample" -- [String]
  let Right firewall = M.fromList <$> (traverse parseLine s) -- firewall :: Firewall
      puzzle1 = runReader run firewall
      puzzle2 = runReader findMinDelay firewall
  puzzle2 `seq` putStrLn "====="
  putStrLn $ "Puzzle 1: total severity is " ++ (show puzzle1) -- 2164
  putStrLn $ "Shortest safe delay is " ++ (show puzzle2)
-- Puzzle 2 - find min delay that produces severity 0. It's not 67 or 66
-- It's not 168108, either; that's too low...

