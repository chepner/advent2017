import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Trans.State
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

-- Function data structres are hard. Keeping this crap just in case I still
-- need to figure it out for Puzzle 2.
--
-- Partition the tree descriptions; leaves get turned into ProcessTree values,
-- TreeDescriptions are passed to Phase Two
buildTreePhaseOne :: [TreeDescription] -> ([ProcessTree], [TreeDescription])
buildTreePhaseOne xs = go ([], []) xs
  where go result [] = result
        go (leaves, trees) ((n,w,[]):xs) = go ((PT n w [] : leaves), trees) xs
        go (leaves, trees) (x:xs) = go (leaves, x:trees) xs

type TreeMap a = M.Map Name a
type Partials = (TreeMap ProcessTree, TreeMap TreeDescription)

buildTree :: TreeDescription -> State Partials ProcessTree
buildTree (n, w, ch) = do
    (pts, tds) <- get
    let fallBack :: Name -> State Partials ProcessTree
        fallBack n = buildTree (M.findWithDefault undefined n tds)
        getChild :: Name -> State Partials ProcessTree
        getChild n = case M.lookup n pts of
                      Just pt -> pure pt
                      Nothing -> buildTree (M.findWithDefault undefined n tds)
    children <- traverse getChild ch
    let newTree = PT n w children
    put (M.insert n newTree pts, tds)
    return newTree
                    

badRootFinder s = let Right (pts, tds) = buildTreePhaseOne <$> traverse parseLine (lines s)
                      initialState = (M.fromList [(n,pt) | pt@(PT n _ _) <- pts],
                                      M.fromList [(n,td) | td@(n, _, _) <- tds])
                      PT n w _ = evalState (buildTree (head tds)) initialState
                  in n

-- Puzzle 1, however, is easier to just walk through an association list or map


findRoot :: [TreeDescription] -> Name
findRoot tds@((n,_,_):_) = go n (tds >>= (\(p,_,cs) -> zip cs (repeat p)))
                           where go c ps = case lookup c ps of
                                            Nothing -> c
                                            Just p -> go p ps
     
puzzle1 s = fmap findRoot (traverse parseLine (lines s))
puzzle2 s = 0

main = do 
       s <- readFile "day7.input"
       print (puzzle1 s) -- mwzaxaj
       print (puzzle2 s)
