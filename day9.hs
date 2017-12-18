module Day9 where

import Text.Parsec as T
import Text.Parsec.Token as Tok
import Data.Functor.Identity

-- Yikes.
-- The ideal group is built recursively from empty sets and
-- containment. Absent garbage, it's just { and }.
-- Garbage complicates it. Gargabe is inside <>, with ! as
-- an escape character. Garbage does not nest.

{-
langdef = Tok.LanguageDef {
  Tok.commentStart = T.char '<'
, Tok.commentEnd = ">"  -- but not !>
, Tok.commentLine = ""
, Tok.nestedComments = False
, Tok.identStart = ""
, Tok.identLetter = ""
, Tok.opStart = ""
, Tok.opLetter = ""
, Tok.reservedNames = ""
, Tok.reservedOpNames = ""
, Tok.caseSensitive = ""
}

lexer :: Tok.GenTokenParser String () Identity
lexer = Tok.makeTokenParser langdef
-}

type Parser = T.Parsec String ()

data Garbage = Garbage String deriving Show
data Group = Group [Either Garbage Group] deriving Show

escape :: Parser String
escape = do
   c1 <- T.char '!'
   c2 <- T.anyChar
   return [c1,c2]

nonEscape :: Parser Char
nonEscape = T.noneOf "!>"

garbageCharacter :: Parser String
garbageCharacter = (fmap return nonEscape) T.<|> (T.try escape)

garbage :: Parser Garbage
garbage = do
  T.char '<'
  rc <- T.many garbageCharacter
  T.char '>'
  return $ Garbage (concat rc)

groupOrGarbage :: Parser (Either Garbage Group)
groupOrGarbage = fmap Right group T.<|> fmap Left garbage

group :: Parser Group
group = do
   T.char '{'
   stuff <- T.sepBy groupOrGarbage (T.char ',')
   T.char '}'
   return (Group stuff)

score :: Group -> Int
score g = score' 1 (Right g)
  where score' :: Int -> Either Garbage Group -> Int
        score' n (Right (Group g)) = n + sum (map (score' (n+1)) g)
        score' n (Left g) = 0
  

main = do
   s <- readFile "day9.input"
   print $ fmap score (parse group "" s) -- 11089
