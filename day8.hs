import Control.Monad.Trans.State
import Control.Monad.Trans.State
import Data.Functor.Identity
import qualified Data.Map.Strict as M
import qualified Text.Parsec as T
import qualified Text.Parsec.Token as Tok

{-
 - After parsing a line, initialize any new registers to 0
 -}

type Registers = M.Map String Integer

langdef = Tok.LanguageDef {
  Tok.commentStart = ""
, Tok.commentEnd = ""
, Tok.commentLine = ""
, Tok.nestedComments = False
, Tok.identStart = T.letter
, Tok.identLetter = T.letter
, Tok.opStart = T.oneOf ">!<="
, Tok.opLetter = T.oneOf ">=<"
, Tok.reservedNames = []
, Tok.reservedOpNames = [">", "<", "!=", "==", "<=", ">="]
, Tok.caseSensitive = False
} 

lexer :: Tok.GenTokenParser String () Identity
lexer = Tok.makeTokenParser langdef

type Parser = T.Parsec String ()
data Instruction = Inc | Dec deriving Show

register :: Parser String
register = T.many T.letter

integer :: Parser Integer
integer = rd <$> (minus T.<|> number)
    where rd = read
          minus = (:) <$> T.char '-' <*> number 
          number = T.many1 T.digit

instruction :: Parser Instruction
instruction = do
   d <- T.string "inc" T.<|> T.string "dec"
   return (if d == "inc" then Inc  else Dec)

{-
condition :: Parser (String, String -> Bool)
condition :: do
  T.string "if"
  srcName <- register
  op <- T.oneOf $ map (Tok.symbol lexer) [">" ,  "<" ,  "!=" ,  "==" ,  ">=" ,  "<=" ]
  val <- T.option (T.char "-") >> (T.many T.digit)  -- Parse an integer
  return (reg, (\x -> x `op` val))

-}

whitespace = Tok.whiteSpace lexer

parseLine :: String -> Either T.ParseError (String, Instruction, Integer, String, String, Integer)
parseLine s = let p = do target <- register
                         dir <- whitespace >> instruction
                         value <- whitespace >>  integer
                         whitespace >> T.string "if"
                         test <- whitespace >> register
                         op <- whitespace >> T.choice (map (T.try . T.string) ["!=", "==", ">=", "<=", ">", "<"])
                         value2 <- whitespace >> integer
                         return (target, dir, value, test, op, value2)
              in T.parse p "" s
                    

 

puzzle1 :: Registers -> Integer
puzzle1 = maximum

puzzle2 s = 0
main = do 
       s <- readFile "day8.input"
       traverse (print . parseLine) (lines s)
       --print $ puzzle1 s
       print $ puzzle2 s

