module NamelessLambdas where

import qualified Parser as P
import Parser (Parser)
import Data.Maybe (fromJust)

data Expression
    = Variable      Int
    | Application   Expression  Expression
    | Lambda        Expression
    deriving (Eq)

instance Show Expression where
    show (Variable x)       = show x
    show (Application m n)  = "(" ++ (show m) ++ " " ++ (show n) ++ ")"
    show (Lambda m)         = "λ" ++ "" ++ (show m)

expressionParser :: Parser Expression
expressionParser = P.spaces *> P.unionl
    [lambdaExprParser, varExprParser, applicationExprParser, bracedExprParser]

bracedExprParser :: Parser Expression
bracedExprParser = (P.char '(' *> expressionParser) <* (P.char ')')

varExprParser :: Parser Expression
varExprParser = fmap Variable P.number

lambdaExprParser :: Parser Expression
lambdaExprParser =
    (P.unionl [P.string "lambda", P.string "\\", P.string "λ"])
    *>
    (fmap Lambda expressionParser)

applicationExprParser :: Parser Expression
applicationExprParser = fmap leftAssoc $ P.many other
    where
        other = P.spaces *> (P.unionl [lambdaExprParser, varExprParser, bracedExprParser])
        leftAssoc = foldl1 Application

parser :: Parser Expression
parser = expressionParser <* P.end

parse :: String -> Maybe Expression
parse = P.parse parser

ul :: String -> Expression
ul = fromJust . parse