{-# LANGUAGE DeriveGeneric #-}

module NamelessLambdas where

import qualified Parser as P
import Parser (Parser)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Hashable
import GHC.Generics (Generic)

type Variable = Int

data Term
    = Variable      Int
    | Application   Term  Term
    | Lambda        Term
    deriving (Eq, Generic)

instance Hashable Term

instance Show Term where
    show (Variable x)       = show x
    show (Application m n)  = "(" ++ (show m) ++ " " ++ (show n) ++ ")"
    show (Lambda m)         = "λ" ++ "" ++ (show m)

instance P.Parseable Term where
    parser = termParser

numFV :: Term -> Int
numFV = Set.size . fv

fv :: Term -> Set Variable
fv (Variable x) = Set.singleton x
fv (Application m n) = Set.union (fv m) (fv n)
fv (Lambda m) = Set.map (\x -> x - 1) (Set.delete 0 $ fv m)

varParser :: Parser Variable
varParser = P.number

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
    (Lambda <$> termParser)

applicationTermParser :: Parser Term
applicationTermParser = fmap leftAssoc $ P.many other
    where
        other = do
            P.spaces
            term <- P.unionl [lambdaTermParser, varTermParser, bracedTermParser]
            P.spaces
            return term

        leftAssoc = foldl1 Application


parser :: Parser Term
parser = termParser <* P.end