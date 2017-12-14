import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import qualified Text.Parsec as T
import qualified Text.Parsec.Token as Tok

type Name = String
type Weight = Int

data ProcessTree = PT Name Weight [ProcessTree] deriving Show

-- Parse each line into a process tree.
-- Merge trees into one large tree
-- Puzzle one asks for the root of the large tree.
-- You could also solve this by finding a LHS arrow name that does
-- not appear after an arrow itself.
--

langdef = Tok.LanguageDef {
  Tok.commentStart = ""
, Tok.commentEnd = ""
, Tok.commentLine = ""
, Tok.nestedComments = False
, Tok.identStart = T.letter
, Tok.identLetter = T.letter
, Tok.opStart = T.char '-'
, Tok.opLetter = T.char '>'
, Tok.reservedNames = []
, Tok.reservedOpNames = []
, Tok.caseSensitive = False
} 

lexer :: Tok.GenTokenParser String () Identity
lexer = Tok.makeTokenParser langdef

type Parser = T.Parsec String ()

name :: Parser Name
name = T.many T.letter

arrow :: Parser String
arrow = Tok.symbol lexer "->"

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer


weight :: Parser Weight
weight = do
      T.char '('
      value <- T.many T.digit
      T.char ')'
      return (read value)

children :: Parser [Name]
children = Tok.commaSep lexer name

type TreeDescription = (Name, Weight, [Name])

parseLine :: String -> Either T.ParseError TreeDescription
parseLine s = let p = do {
                n <- name;
                w <- whitespace >> weight;
                ch <- whitespace >> T.option [] (arrow >> children);
                return (n,w,ch);
                }

              in T.parse p "" s


-- Puzzle 1, find the root of the tree. Just pick an arbitrary
-- child and walk up.

findRoot :: [TreeDescription] -> Name
findRoot tds@((n,_,_):_) = go n parents
                           where parents = tds >>= \(p,_,cs) -> zip cs (repeat p)
                                 go c ps = case lookup c ps of
                                            Nothing -> c
                                            Just p -> go p ps
     
parseDescriptions :: String -> Either T.ParseError [TreeDescription]
parseDescriptions = traverse parseLine . lines

puzzle1 :: [TreeDescription] -> Name
puzzle1 = findRoot

--- Puzzle 2 - find the node with an incorrect weight,
--  and determine the weight that will balance the tree
--


puzzle2 tds = 0

main = do 
       s <- readFile "day7.input"
       let Right tds = parseDescriptions s
       let root = puzzle1 tds -- mwzaxaj
       print root
       print (puzzle2 s)
