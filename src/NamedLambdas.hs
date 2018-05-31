module NamedLambdas where

import qualified Parser as P
import Parser (Parser)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

type Variable = String

data Expression
    = Variable      Variable
    | Application   Expression  Expression
    | Lambda        Variable    Expression
    deriving (Eq)

fvList :: Expression -> [Variable]
fvList = (Set.elems . fv)

fv :: Expression -> Set Variable
fv (Variable x) = Set.singleton x
fv (Application m n) = Set.union (fv m) (fv n)
fv (Lambda x m) = Set.delete x (fv m)

bvList :: Expression -> [Variable]
bvList = (Set.elems . bv)

bv :: Expression -> Set Variable
bv (Variable x) = Set.empty
bv (Application m n) = Set.union (bv m) (bv n)
bv (Lambda x m) = Set.insert x (bv m)

instance Show Expression where
    show (Variable x)       = x
    show (Application m n)  = "(" ++ (show m) ++ " " ++ (show n) ++ ")"
    show (Lambda x m)       = "λ" ++ x ++ "" ++ (show m)

instance P.Parseable Expression where
    parser = expressionParser

varChars = "xyztuvwpqrklsmnijabcdefgho"

allVars :: [Variable]
allVars = (map pure) varChars ++ (map (('x' : ) . show) [1..])

varParser :: Parser Variable
varParser =
    do
        P.spaces
        x <- P.charOf varChars
        n <- P.fallback "" $ do
            num <- P.number
            return $ show num
        return $ x : n

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

stupidParser :: Parser Char
stupidParser = P.union
    ((P.char ',') *> stupidParser)
    (P.char 'щ')

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
    (P.concatenate Lambda varParser expressionParser)

applicationExprParser :: Parser Expression
applicationExprParser = fmap leftAssoc $ P.many other
    where
        other = do
            P.spaces
            term <- P.unionl [lambdaExprParser, varExprParser, bracedExprParser]
            P.spaces
            return term

        leftAssoc = foldl1 Application