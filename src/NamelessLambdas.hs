module NamelessLambdas where

import qualified Parser as P
import Parser (Parser)
import Data.Maybe (fromJust)

type Variable = Int

data Expression
    = Variable      Int
    | Application   Expression  Expression
    | Lambda        Expression
    deriving (Eq)

instance Show Expression where
    show (Variable x)       = show x
    show (Application m n)  = "(" ++ (show m) ++ " " ++ (show n) ++ ")"
    show (Lambda m)         = "λ" ++ "" ++ (show m)

instance P.Parseable Expression where
    parser = expressionParser

varParser :: Parser Variable
varParser = P.number

expressionParser :: Parser Expression
expressionParser = do
    P.spaces

    term <- P.unionl [
        lambdaExprParser,
        applicationExprParser,
        varExprParser,
        bracedExprParser]

    P.spaces
    return term


bracedExprParser :: Parser Expression
bracedExprParser = do
    P.char '('
    term <- expressionParser
    P.char ')'
    return term

varExprParser :: Parser Expression
varExprParser = fmap Variable varParser

lambdaExprParser :: Parser Expression
lambdaExprParser =
    (P.unionl [P.string "lambda", P.string "\\", P.string "λ"])
    *>
    (Lambda <$> expressionParser)

applicationExprParser :: Parser Expression
applicationExprParser = fmap leftAssoc $ P.many other
    where
        other = do
            P.spaces
            term <- P.unionl [lambdaExprParser, varExprParser, bracedExprParser]
            P.spaces
            return term

        leftAssoc = foldl1 Application


parser :: Parser Expression
parser = expressionParser <* P.end