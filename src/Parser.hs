module Parser where

import Data.Char (ord)
import Data.Maybe (listToMaybe)

type ParseResult a = [(a, String)]

type Parser a = String -> ParseResult a

parse :: Parser a -> String -> Maybe a
parse p s = fst <$> (listToMaybe $ p s)

pmap :: (a -> b) -> Parser a -> Parser b
pmap f p s = (\(y, rest) -> (f y, rest)) <$> (p s)

rmap :: (a -> b) -> ParseResult a -> ParseResult b
rmap = fmap . (\f (x, rest) -> (f x, rest))

yield :: a -> Parser a
yield x s = pure (x, s)

end :: Parser ()
end "" = pure ((), "")
end _  = []

concatenate :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
concatenate f pa pb s = do
    (x, intermediate) <- pa s
    (y, rest)         <- pb intermediate
    return $ (f x y, rest)

compose :: Parser a -> (a -> Parser b) -> Parser b
compose pa f s = do
    (x, intermediate) <- pa s
    let pb = f x
    (y, rest)         <- pb intermediate
    return $ (y, rest)

left :: Parser a -> Parser b -> Parser a
left = concatenate (\x _ -> x)

right :: Parser a -> Parser b -> Parser b
right = concatenate (\_ y -> y)

union :: Parser a -> Parser a -> Parser a
union pa pb s = (pa s) ++ (pb s)

many :: Parser a -> Parser [a]
many p = concatenate (:) p (manyOrNone p)

manyOrNone :: Parser a -> Parser [a]
manyOrNone p s =
    (concat $ map (\(x, rest) -> rmap (x :) (manyOrNone p rest)) $ p s)
    ++ [([], s)]

char :: Char -> Parser Char
char c (x:xs)
    | c == x    = [(c, xs)]
    | otherwise = []
char _ [] = []

space :: Parser Char
space = charOf " \n\r\t"

spaces :: Parser String
spaces = union (many space) (yield "")

string :: String -> Parser String
string = (foldr (concatenate (:)) (yield [])) . (map char)

charOf :: String -> Parser Char
charOf = (foldr1 union) . (map char)

number :: Parser Int
number = pmap digitsToNumber $ many digit
    where
        digitsToNumber = foldl1 (\n d -> n * 10 + d)

digit :: Parser Int
digit (x:xs)
    | x >= '0' && x <= '9'  = [((ord x) - (ord '0'), xs)]
    | otherwise             = []
digit [] = []