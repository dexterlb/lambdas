module ProtoParser where

import Data.Char (ord)
import Data.Maybe (listToMaybe)

type ParseResult a = [(a, String)]

type Parser a = String -> ParseResult a

parse :: Parser a -> String -> Maybe a
parse p s = fst <$> (listToMaybe $ p s)

allResults :: Parser a -> String -> [(a, String)]
allResults p s = p s

pmap :: (a -> b) -> Parser a -> Parser b
pmap f p s = (\(y, rest) -> (f y, rest)) <$> (p s)

rmap :: (a -> b) -> ParseResult a -> ParseResult b
rmap = fmap . (\f (x, rest) -> (f x, rest))

yield :: a -> Parser a
yield x s = pure (x, s)

end :: Parser ()
end "" = pure ((), "")
end _  = []

compose :: Parser a -> (a -> Parser b) -> Parser b
compose pa f s = do
    (x, intermediate) <- pa s
    let pb = f x
    (y, rest)         <- pb intermediate
    return $ (y, rest)

right :: Parser a -> Parser b -> Parser b
right pa pb s = do
    (_, intermediate) <- pa s
    (y, rest)         <- pb intermediate
    return $ (y, rest)

union :: Parser a -> Parser a -> Parser a
union pa pb s = (pa s) ++ (pb s)

unionl :: [Parser a] -> Parser a
unionl = foldr1 union

manyOrNone :: Parser a -> Parser [a]
manyOrNone p s =
    (concat $ map (\(x, rest) -> rmap (x :) (manyOrNone p rest)) $ p s)
    ++ [([], s)]

char :: Char -> Parser Char
char c (x:xs)
    | c == x    = [(c, xs)]
    | otherwise = []
char _ [] = []

digit :: Parser Int
digit (x:xs)
    | x >= '0' && x <= '9'  = [((ord x) - (ord '0'), xs)]
    | otherwise             = []
digit [] = []