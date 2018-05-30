{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Procedures where

import qualified NamedLambdas as NL
import qualified NamelessLambdas as UL
import Lambdas
import Data.List (intercalate)
import qualified Parser as P
import Parser (Parser)
import Control.Applicative (liftA2)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Control.Monad (liftM2)

type Cache = HashMap Identifier UL.Expression

type Identifier = String
type Arity = Int

data Declaration = Declaration String Context deriving (Eq)

instance Show Declaration where
    show (Declaration f vars) = f ++ "(" ++ (intercalate ", " vars) ++ ")"

instance P.Parseable Declaration where
    parser = fparser Declaration NL.varParser

identifierParser = do
    fh <- P.charOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    ft <- P.manyOrNone $ P.charOf "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890_"
    return $ fh : ft

fparser :: (Identifier -> [a] -> b) -> Parser a -> Parser b
fparser constructor item = do
    P.spaces
    f <- identifierParser

    vars <- P.fallback [] $ do
        P.spaces
        P.char '('
        P.spaces
        vars <- commaParser item
        P.char ')'
        return vars

    return $ constructor f vars

commaParser :: Parser a -> Parser [a]
commaParser p = do
    itemh <- p
    itemt <- P.manyOrNone $ do
        P.spaces
        P.char ','
        P.spaces
        item <- p
        P.spaces
        return item
    return $ itemh : itemt

data Procedure = Procedure Declaration Expression

instance Show Procedure where
    show (Procedure call body)
        = (show call) ++ " = " ++ (show body)

data Expression
    = Variable      NL.Variable
    | Application   Expression  Expression
    | Lambda        NL.Variable Expression
    | Call          Identifier  [Expression]
    deriving (Eq)

instance Show Expression where
    show (Variable x)       = x
    show (Application m n)  = "(" ++ (show m) ++ " " ++ (show n) ++ ")"
    show (Lambda x m)       = "λ" ++ x ++ "" ++ (show m)
    show (Call f terms)     = f ++ "(" ++ (intercalate ", " (map show terms)) ++ ")"

instance P.Parseable Expression where
    parser = expressionParser

fromNamed :: NL.Expression -> Expression
fromNamed (NL.Variable x)       = Variable x
fromNamed (NL.Application m n)  = Application (fromNamed m) (fromNamed n)
fromNamed (NL.Lambda x m)       = Lambda x (fromNamed m)

toUnnamed :: Context -> Cache -> Expression -> Maybe UL.Expression
toUnnamed c _  (Variable x)         =        UL.Variable <$> index c x
toUnnamed c ch (Application m n)    = liftM2 UL.Application  (toUnnamed c ch m) (toUnnamed c ch n)
toUnnamed c ch (Lambda x m)         =        UL.Lambda   <$> toUnnamed (push x c) ch m
toUnnamed c ch (Call f terms)       = replaceCall (mapM (toUnnamed c ch) terms) =<< H.lookup f ch
    where
        replaceCall :: Maybe [UL.Expression] -> UL.Expression -> Maybe UL.Expression
        replaceCall (Just unnamedTerms) expr
            | UL.numFV expr == length terms = Just $ replaceCall' 0 unnamedTerms expr
            | otherwise = Nothing
        replaceCall Nothing _ = Nothing

        replaceCall' :: Int -> [UL.Expression] -> UL.Expression -> UL.Expression
        replaceCall' _ [] expr = expr
        replaceCall' i (x:xs) expr = replaceCall' (i + 1) xs expr'
            where
                expr' = substitute expr i x


expressionParser :: Parser Expression
expressionParser = do
    P.spaces

    term <- P.unionl [
        lambdaExprParser,
        applicationExprParser,
        varExprParser,
        callExprParser,
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
varExprParser = fmap Variable NL.varParser

lambdaExprParser :: Parser Expression
lambdaExprParser =
    (P.unionl [P.string "lambda", P.string "\\", P.string "λ"])
    *>
    (P.concatenate Lambda NL.varParser expressionParser)

applicationExprParser :: Parser Expression
applicationExprParser = fmap leftAssoc $ P.many other
    where
        other = do
            P.spaces
            term <- P.unionl [lambdaExprParser, varExprParser, callExprParser, bracedExprParser]
            P.spaces
            return term

        leftAssoc = foldl1 Application

callExprParser :: Parser Expression
callExprParser = fparser Call P.parser