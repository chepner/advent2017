import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import qualified Text.Parsec as T
import qualified Text.Parsec.Token as Tok
import qualified Data.Set as S

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


data RoseTree a = RT a [RoseTree a]

printRoseTree :: Show a => RoseTree a -> IO ()
  
printRoseTree rt =  let printList [] = return ()
                        printList (x:xs) = putStrLn x >> printList xs
                        withIndent pfx (RT root ch) = (pfx ++ show root) : (ch >>= (withIndent (' ':pfx)))
                    in printList (withIndent "" rt)

buildTree :: Name -> Reader [(Name, TreeDescription)] (RoseTree (Name, Weight, Weight, [Weight]))
buildTree n = do
   tds <- ask
   let (name, weight, ch) = maybe undefined id (lookup n tds)
   children <- traverse buildTree ch
   --let ss = weight + sum . map (\(RT (n, w, ss) _) -> ss) $ children
   let getStackSize (RT (_, _, ss, _) _) = ss
       childStackSizes = map getStackSize children
   return (RT (name, weight, weight + sum childStackSizes, childStackSizes) children)


{-
 - OK, here's how I solved Puzzle 2. Printing out the entire tree, I noted the root had
 - the following child stack sizes: [67398,67398,67398,67398,67405,67398,67398] Looking
 - for the child with the odd size 67405, I find *its* odd child and repeat. Eventually,
 - I got to  ("ihnus",40,15160,[2166,2159,2159,2159,2159,2159,2159])
 -            ("vrgxe",1226,2166,[235,235,235,235])
 - from which you can see that vrgxe needs a weight of 1226 - 7 = 1219 so that all
 - of ihnus's children have a size of 2166 - 7 = 2159.
 -
 - Ugh. I'm sick of this puzzle. I'm not implementing the above search just yet.
 -}
main = do 
       s <- readFile "day7.input"
       let Right tds = parseDescriptions s
       let root = puzzle1 tds -- mwzaxaj
       print root
       let m = [(n, td) | td@(n,_,_) <- tds]
           rt = runReader (buildTree root) m
       printRoseTree rt
