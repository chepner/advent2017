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
                       return (fromIntegral depth, fromIntegral range)
            in T.parse p ""
                   

type Depth = Int
type Range = Int
type Time = Int
type Severity = Int
type FirewallLayer = (Depth, Range)
type Firewall = M.Map Depth Range

severity :: Time -> Reader Firewall Severity
severity t = do
   fw <- ask
   let period r = 2 * r - 2
   return $ case M.lookup t fw of
             Nothing -> 0 -- No scanner at this depth
             Just p ->  if t `mod` (period p) == 0 then p * t else 0

-- r == 1: 0 0 0 0 0 0  ...  (p = 0)
-- r == 2: 0 1 0 1 0 1  ...  (p = 2)
-- r == 3: 0 1 2 1 0 1  ...  (p = 4)
-- r == 4: 0 1 2 3 2 1  ...  (p = 6)
-- r'    : 0 1 ... r-2 r-1 r-2 ... (p = 2(r-1) = 2r - 2)

main = do
  s <- lines <$> readFile "day13.input" -- [String]
  let Right firewall = M.fromList <$> (traverse parseLine s) -- firewall :: Firewall
      maxD = maximum $ M.keys firewall
      severities = traverse severity [0..maxD - 1]
  print $ sum ( runReader severities firewall) -- 2164

