module Day11 where

import Data.List.Split
import Data.Foldable

data Hex = Hex {x::Int, y::Int, z::Int} deriving Show

step :: Hex -> String -> Hex
step h "n" = h { z=z h - 1} {y = y h + 1}
step h "ne" = h { x = x h+1} {z = z h - 1}
step h "se" = h {x = x h + 1} {y = y h - 1}
step h "s" = h {y = y h - 1} {z = z h + 1}
step h "sw" = h {x = x h - 1} {z = z h + 1}
step h "nw" = h {x = x h - 1} {y = y h + 1}

main = do
  s <- readFile "day11.input"
  let dirs = splitOn "," . init $ s
  let final = foldl' step (Hex 0 0 0) dirs
  print $ ((abs .x) final + (abs.y) final + (abs.z) final) `div` 2
  -- 1763 is too high
  -- 687 is right.
