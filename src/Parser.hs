module Parser where

import Data.Char (ord)
import Data.Maybe (listToMaybe)

type Void = ()
void :: Void
void = ()

type ParseResult a = [(a, String)]

type Parser a = String -> ParseResult a

parse :: Parser a -> String -> Maybe a
parse p s = fst <$> (listToMaybe $ p s)

pmap :: (a -> b) -> Parser a -> Parser b
pmap f p s = (\(y, rest) -> (f y, rest)) <$> (p s)

rmap :: (a -> b) -> ParseResult a -> ParseResult b
rmap f = map (\(a, rest) -> (f a, rest))

yield :: a -> Parser a
yield x s = pure (x, s)

end :: Parser Void
end "" = [(void, "")]
end _  = []

con :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
con f pa pb s = do
    (x, intermediate) <- pa s
    (y, rest)         <- pb intermediate
    return $ (f x y, rest)

left :: Parser a -> Parser b -> Parser a
left = con (\x _ -> x)

right :: Parser a -> Parser b -> Parser b
right = con (\_ y -> y)

union :: Parser a -> Parser a -> Parser a
union pa pb s = (pa s) ++ (pb s)

unionl :: [Parser a] -> Parser a
unionl = foldr1 union

many :: Parser a -> Parser [a]
many p = union (pmap pure p) (manyOrNone p)

manyOrNone :: Parser a -> Parser [a]
manyOrNone p s = concat $ map (\(x, rest) -> rmap (x :) (manyOrNone p rest)) $ p s

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
string = (foldr (con (:)) (yield [])) . (map char)

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