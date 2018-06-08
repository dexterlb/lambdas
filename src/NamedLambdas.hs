module NamedLambdas where

import qualified Parser as P
import Parser (Parser)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

type Variable = String

data Term
    = Variable      Variable
    | Application   Term  Term
    | Lambda        Variable    Term
    deriving (Eq)

fvList :: Term -> [Variable]
fvList = (Set.elems . fv)

fv :: Term -> Set Variable
fv (Variable x) = Set.singleton x
fv (Application m n) = Set.union (fv m) (fv n)
fv (Lambda x m) = Set.delete x (fv m)

bvList :: Term -> [Variable]
bvList = (Set.elems . bv)

bv :: Term -> Set Variable
bv (Variable x) = Set.empty
bv (Application m n) = Set.union (bv m) (bv n)
bv (Lambda x m) = Set.insert x (bv m)

instance Show Term where
    show (Variable x)       = x
    show (Application m n)  = "(" ++ (show m) ++ " " ++ (show n) ++ ")"
    show (Lambda x m)       = "λ" ++ x ++ "" ++ (show m)

instance P.Parseable Term where
    parser = termParser

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

termParser :: Parser Term
termParser = do
    P.spaces

    term <- P.unionl [
        lambdaTermParser,
        applicationTermParser,
        varTermParser,
        bracedTermParser]

    P.spaces
    return term

stupidParser :: Parser Char
stupidParser = P.union
    ((P.char ',') *> stupidParser)
    (P.char 'щ')

bracedTermParser :: Parser Term
bracedTermParser = do
    P.char '('
    term <- termParser
    P.char ')'
    return term

varTermParser :: Parser Term
varTermParser = fmap Variable varParser

lambdaTermParser :: Parser Term
lambdaTermParser =
    (P.unionl [P.string "lambda", P.string "\\", P.string "λ"])
    *>
    (P.concatenate Lambda varParser termParser)

applicationTermParser :: Parser Term
applicationTermParser = fmap leftAssoc $ P.many other
    where
        other = do
            P.spaces
            term <- P.unionl [lambdaTermParser, varTermParser, bracedTermParser]
            P.spaces
            return term

        leftAssoc = foldl1 Application