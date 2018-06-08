{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Procedures where

import qualified NamedLambdas as NL
import qualified NamelessLambdas as UL
import Lambdas
import Maths
import Data.List (intercalate)
import qualified Parser as P
import Parser (Parser)
import Control.Applicative (liftA2)
import Control.Monad (liftM2)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Cache (Cache, Identifier)
import qualified Cache as C

import Debug.Trace

type Arity = Int

data Declaration = Declaration String Context deriving (Eq)

instance Show Declaration where
    show (Declaration f [])     = f
    show (Declaration f vars)   = f ++ "(" ++ (intercalate ", " vars) ++ ")"

instance P.Parseable Declaration where
    parser = fparser Declaration NL.varParser

data Definition = Definition Declaration Term

instance Show Definition where
    show (Definition declaration body) = (show declaration) ++ " := " ++ (show body)

instance P.Parseable Definition where
    parser = do
        declaration <- P.parser
        P.spaces
        P.string ":="
        P.spaces
        body <- P.parser

        return $! Definition declaration body

makeDefinition :: Identifier -> UL.Term -> Definition
makeDefinition f term = Definition declaration body
    where
        vars = fvList body
        declaration = Declaration f vars
        body = fromJust $ fromNamed <$> name (take (UL.numFV term) NL.allVars) term

identifierParser = do
    fh <- P.charOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    ft <- P.manyOrNone $ P.charOf "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890_"
    return $ fh : ft

define :: Definition -> Cache -> Maybe Cache
define d c = do
    let (Definition (Declaration f vars) body) = d
    term <- apply d c
    return $ C.put f term c

defineReduce :: Int -> Definition -> Cache -> Maybe (Cache, ProcessResult)
defineReduce n d c = do
    let (Definition (Declaration f vars) body) = d
    term <- apply d c
    let (reducedTerm, result) = reduceUntil n term
    return $ (C.put f reducedTerm c, result)


apply :: Definition -> Cache -> Maybe UL.Term
apply (Definition (Declaration f vars) body) cache = toUnnamed vars cache body

fvList :: Term -> [NL.Variable]
fvList = (Set.elems . fv)

fv :: Term -> Set NL.Variable
fv (Variable x)      = Set.singleton x
fv (Application m n) = Set.union (fv m) (fv n)
fv (Lambda x m)      = Set.delete x (fv m)
fv (Call _ terms)    = foldr Set.union Set.empty (map fv terms)
fv (ChurchNumeral _) = Set.empty

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

data Procedure = Procedure Declaration Term

instance Show Procedure where
    show (Procedure call body)
        = (show call) ++ " = " ++ (show body)

data Term
    = Variable      NL.Variable
    | Application   Term  Term
    | Lambda        NL.Variable Term
    | Call          Identifier  [Term]
    | ChurchNumeral Int
    deriving (Eq)

instance Show Term where
    show (Variable x)       = x
    show (Application m n)  = "(" ++ (show m) ++ " " ++ (show n) ++ ")"
    show (Lambda x m)       = "λ" ++ x ++ "" ++ (show m)
    show (Call f [])        = f
    show (Call f terms)     = f ++ "(" ++ (intercalate ", " (map show terms)) ++ ")"
    show (ChurchNumeral n)  = "#" ++ (show n)

instance P.Parseable Term where
    parser = termParser

fromNamed :: NL.Term -> Term
fromNamed (NL.Variable x)       = Variable x
fromNamed (NL.Application m n)  = Application (fromNamed m) (fromNamed n)
fromNamed (NL.Lambda x m)       = Lambda x (fromNamed m)

toUnnamed :: Context -> Cache -> Term -> Maybe UL.Term
toUnnamed c _  (Variable x)         =        UL.Variable <$> index c x
toUnnamed c ch (Application m n)    = liftM2 UL.Application  (toUnnamed c ch m) (toUnnamed c ch n)
toUnnamed c ch (Lambda x m)         =        UL.Lambda   <$> toUnnamed (push x c) ch m
toUnnamed c ch (Call f terms)       = replaceCall (mapM (toUnnamed c ch) terms) =<< C.get f ch
    where
        replaceCall :: Maybe [UL.Term] -> UL.Term -> Maybe UL.Term
        replaceCall (Just unnamedTerms) term
            | UL.numFV term == length terms = Just $ replaceCall' 0 unnamedTerms term
            | otherwise = Nothing
        replaceCall Nothing _ = Nothing

        replaceCall' :: Int -> [UL.Term] -> UL.Term -> UL.Term
        replaceCall' _ [] term = term
        replaceCall' i (x:xs) term = replaceCall' (i + 1) xs term'
            where
                term' = substitute term i x
toUnnamed c _ (ChurchNumeral n) = Just $ UL.Lambda $ UL.Lambda (fs (UL.Variable 0))
    where
        fs = foldr (.) id (replicate n (UL.Application $ UL.Variable 1))

fromUnnamed :: Cache -> Context -> UL.Term -> Maybe Term
fromUnnamed h c e
    | (Just f) <- C.getBackward e h     = Just $ Call f []
    | (Just n) <- decodeChurchNumeral e = Just $ ChurchNumeral n
    | otherwise = fromNamed <$> (name c e)

termParser :: Parser Term
termParser = do
    P.spaces

    term <- P.unionl [
        churchNumeralTermParser,
        lambdaTermParser,
        applicationTermParser,
        varTermParser,
        callTermParser,
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
varTermParser = fmap Variable NL.varParser

lambdaTermParser :: Parser Term
lambdaTermParser =
    (P.unionl [P.string "lambda", P.string "\\", P.string "λ"])
    *>
    (P.concatenate Lambda NL.varParser termParser)

applicationTermParser :: Parser Term
applicationTermParser = fmap leftAssoc $ P.many other
    where
        other = do
            P.spaces
            term <- P.unionl
                [ churchNumeralTermParser
                , lambdaTermParser
                , varTermParser
                , callTermParser
                , bracedTermParser
                ]
            P.spaces
            return term

        leftAssoc = foldl1 Application

callTermParser :: Parser Term
callTermParser = fparser Call P.parser

churchNumeralTermParser :: Parser Term
churchNumeralTermParser = P.char '#' *> (ChurchNumeral <$> P.number)

decodeChurchNumeral :: UL.Term -> Maybe Int
decodeChurchNumeral (UL.Lambda (UL.Lambda f)) = decodeApplication f
    where
        decodeApplication (UL.Variable 0) = Just 0
        decodeApplication (UL.Application (UL.Variable 1) rest) = (1 +) <$> decodeApplication rest
        decodeApplication _ = Nothing
decodeChurchNumeral _ = Nothing

showCache :: Cache -> String
showCache c =
    "\n--------\n" ++
    (intercalate "\n" $ map (show . (uncurry makeDefinition)) $ C.toList $ c)
    ++ "\n--------\n"