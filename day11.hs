module Day11 where

import Data.List.Split
import Data.Foldable
import Control.Monad.Trans.Writer
import Control.Monad

data Hex = Hex {x::Int, y::Int, z::Int} deriving Show

step :: Hex -> String -> Writer [Int] Hex
step h dir = do
   let next = step' h dir
   tell [distance (Hex 0 0 0) next]
   return next
 where
   step' h "n" = h { z=z h - 1} {y = y h + 1}
   step' h "ne" = h { x = x h+1} {z = z h - 1}
   step' h "se" = h {x = x h + 1} {y = y h - 1}
   step' h "s" = h {y = y h - 1} {z = z h + 1}
   step' h "sw" = h {x = x h - 1} {z = z h + 1}
   step' h "nw" = h {x = x h - 1} {y = y h + 1}

distance :: Hex -> Hex -> Int
distance h1 h2 = (abs (x h1 - x h2) + abs (y h1 - y h2) + abs (z h1 - z h2)) `div` 2

main = do
  s <- readFile "day11.input"
  let dirs = splitOn "," . init $ s
      initial = Hex 0 0 0
  let final = foldM step initial dirs
  print $ maximum ( execWriter final)
  -- 1483 is right!
