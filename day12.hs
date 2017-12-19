module Day12 where

import qualified Text.Parsec as T
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Graph as G

lexer = Tok.makeTokenParser emptyDef
whitespace = Tok.whiteSpace lexer
node = Tok.lexeme lexer (Tok.natural lexer)
arrow = Tok.symbol lexer "<->"

type Parser = T.Parsec String ()

parseLine :: String -> Either T.ParseError (Integer, [Integer])
parseLine = let p = do whitespace
                       src <- node
                       arrow
                       adj <- Tok.commaSep lexer node
                       return (src, adj)
            in T.parse p ""


main = do
  s <- readFile "day12.input"
  let Right adjList = traverse parseLine (lines s)
      (graph, _, _) = G.graphFromEdges [(n,n,e) | (n, e) <- adjList ]
      reachableCount = map length $ G.dfs graph [0]
  print reachableCount -- 169
