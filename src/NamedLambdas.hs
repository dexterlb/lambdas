module NamedLambdas where

import qualified Parser as P
import Parser (Parser)
import Data.Maybe (fromJust)

type Variable = String

data Expression
    = Variable      Variable
    | Application   Expression  Expression
    | Lambda        Variable    Expression
    deriving (Eq)

instance Show Expression where
    show (Variable x)       = x
    show (Application m n)  = "(" ++ (show m) ++ " " ++ (show n) ++ ")"
    show (Lambda x m)       = "λ" ++ x ++ "" ++ (show m)

varChars = "xyztuvwpqrklsmnijabcdefgho"

allVars :: [Variable]
allVars = (map pure) varChars ++ (map (('x' : ) . show) [1..])


varParser :: Parser Variable
varParser = P.spaces `P.right` P.union
    (P.con (\c n -> c : (show n)) (P.charOf varChars) P.number)
    (P.pmap (\c -> [c]) $ P.charOf varChars)

expressionParser :: Parser Expression
expressionParser = P.spaces `P.right` P.unionl
    [lambdaExprParser, applicationExprParser, varExprParser, bracedExprParser]

bracedExprParser :: Parser Expression
bracedExprParser = (P.char '(' `P.right` expressionParser) `P.left` (P.char ')')

varExprParser :: Parser Expression
varExprParser = P.pmap Variable varParser

lambdaExprParser :: Parser Expression
lambdaExprParser =
    (P.unionl [P.string "lambda", P.string "\\", P.string "λ"])
    `P.right`
    (P.con Lambda varParser expressionParser)

applicationExprParser :: Parser Expression
applicationExprParser = P.pmap leftAssoc $ P.many other
    where
        other = P.spaces `P.right` (P.unionl [lambdaExprParser, varExprParser, bracedExprParser])
        leftAssoc = foldl1 Application


parser :: Parser Expression
parser = expressionParser `P.left` P.end

parse :: String -> Maybe Expression
parse = P.parse parser

nl :: String -> Expression
nl = fromJust . parse