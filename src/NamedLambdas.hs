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

varChars = "xyztuvwpqrklsmnijabcdefgho"

allVars :: [Variable]
allVars = (map pure) varChars ++ (map (('x' : ) . show) [1..])


varParser :: Parser Variable
varParser = P.spaces *> P.union
    (P.concatenate (\c n -> c : (show n)) (P.charOf varChars) P.number)
    (fmap (\c -> [c]) $ P.charOf varChars)

expressionParser :: Parser Expression
expressionParser = P.spaces *> P.unionl
    [lambdaExprParser, applicationExprParser, varExprParser, bracedExprParser]

bracedExprParser :: Parser Expression
bracedExprParser = (P.char '(' *> expressionParser) <* (P.char ')')

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
        other = P.spaces *> (P.unionl [lambdaExprParser, varExprParser, bracedExprParser])
        leftAssoc = foldl1 Application


parser :: Parser Expression
parser = expressionParser <* P.end

parse :: String -> Maybe Expression
parse = P.parse parser

nl :: String -> Expression
nl = fromJust . parse