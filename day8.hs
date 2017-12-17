import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.State
import Data.Functor.Identity
import qualified Data.Map.Strict as M
import qualified Text.Parsec as T
import qualified Text.Parsec.Token as Tok


type Registers = M.Map String Integer

{- Language parser -}

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
data ModOp = Inc | Dec deriving Show

register :: Parser String
register = Tok.identifier lexer

integer :: Parser Integer
integer = Tok.integer lexer

instruction :: Parser ModOp
instruction = do
   d <- T.string "inc" T.<|> T.string "dec"
   return (if d == "inc" then Inc  else Dec)

type CompOp = Integer -> Integer -> Bool

compOp :: Parser CompOp
compOp = do
    op <- whitespace >> T.choice (map (T.try . T.string) ["!=", "==", ">=", "<=", ">", "<"])
    return $ case op of
        "==" -> (==)
        "!=" -> (/=)
        ">" -> (>)
        "<" -> (<)
        ">=" -> (>=)
        "<=" -> (<=)

whitespace = Tok.whiteSpace lexer

type Instruction = (String, ModOp, Integer, String, CompOp, Integer)

parseLine :: String -> Either T.ParseError Instruction
parseLine s = let p = do target <- register
                         dir <- whitespace >> instruction
                         value <- whitespace >>  integer
                         whitespace >> T.string "if"
                         test <- whitespace >> register
                         op <- compOp
                         value2 <- whitespace >> integer
                         return (target, dir, value, test, op, value2)
              in T.parse p "" s


{- Language evaluator -}
evalLine :: Instruction -> State (Registers, Maybe Integer) ()
evalLine (target, op1, val1, test, op2, val2) = do
    (regs, currMax) <- get
    let targVal = M.findWithDefault 0 target regs
        condition = (M.findWithDefault 0 test regs) `op2` val2
        replVal = if condition then val1 else 0
        newVal = case op1 of
                   Inc -> targVal + replVal
                   Dec -> targVal - replVal
        newRegs = M.insert target newVal regs
        newMax = Just $ maybe newVal (max newVal) currMax
    put (M.insert target newVal regs, newMax)


type Program = [Instruction]

evalProgram :: Program -> (Registers, Maybe Integer)
evalProgram pg = execState (foldM (const evalLine) () pg) (M.empty, Nothing)

puzzle :: Program -> Maybe Integer
puzzle = snd . evalProgram

puzzle2 s = 0

main = do 
       s <- readFile "day8.input"
       let Right program = traverse parseLine (lines s)
       print $ puzzle program -- 6366

