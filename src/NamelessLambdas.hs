module NamelessLambdas where

import qualified Parser as P
import Parser (Parser)

data Expression
    = Variable      Int
    | Application   Expression  Expression
    | Lambda        Expression

instance Show Expression where
    show (Variable x)       = show x
    show (Application m n)  = "(" ++ (show m) ++ " " ++ (show n) ++ ")"
    show (Lambda m)         = "λ" ++ "" ++ (show m)

expressionParser :: Parser Expression
expressionParser = P.spaces `P.right` P.unionl
    [lambdaExprParser, varExprParser, applicationExprParser, bracedExprParser]

bracedExprParser :: Parser Expression
bracedExprParser = (P.char '(' `P.right` expressionParser) `P.left` (P.char ')')

varExprParser :: Parser Expression
varExprParser = P.pmap Variable P.number

lambdaExprParser :: Parser Expression
lambdaExprParser =
    (P.unionl [P.string "lambda", P.string "\\", P.string "λ"])
    `P.right`
    (P.pmap Lambda expressionParser)

applicationExprParser :: Parser Expression
applicationExprParser = P.union
    (P.con leftAssoc other applicationExprParser)
    (P.con Application other other)

    where
        other = P.spaces `P.right` (P.unionl [lambdaExprParser, varExprParser, bracedExprParser])
        leftAssoc p (Application m n) = Application (Application p m) n


parser :: Parser Expression
parser = expressionParser `P.left` P.end

parse :: String -> Maybe Expression
parse = P.parse parser