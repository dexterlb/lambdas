module Parser where

import qualified ProtoParser as P
import Control.Applicative (liftA2)

data Parser a = Parser (P.Parser a)     -- typeception

-- instances on type aliases would have made life a whole lot easier

instance Monad Parser where
    return x                = Parser (P.yield x)
    Parser p >> Parser q    = Parser $ p `P.right` q
    Parser p >>= f          = Parser $ P.compose p (dropF f)

instance Applicative Parser where
    pf <*> p = do
        f <- pf
        x <- p
        return $ f x
    pure = return

instance Functor Parser where
    fmap f p = do
        x <- p
        return $ f x

class Parseable p where
    parser :: Parser p

parse :: Parseable a => String -> Maybe a
parse = parseWith (fullParser parser)

parsePrefix :: Parseable a => String -> Maybe a
parsePrefix = parseWith parser

fullParser :: Parser a -> Parser a
fullParser p = p <* end

parseWith :: Parser a -> String -> Maybe a
parseWith (Parser p) s = P.parseWith p s

allResults :: Parser a -> String -> [(a, String)]
allResults (Parser p) s = P.allResults p s

end :: Parser ()
end = Parser P.end

concatenate :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
concatenate = liftA2

unionH :: Parser a -> Parser b -> Parser (Either a b)
unionH p q = union (Left <$> p) (Right <$> q)

union :: Parser a -> Parser a -> Parser a
union = lift2 P.union

unionl :: [Parser a] -> Parser a
unionl = foldr1 union

fallback :: a -> Parser a -> Parser a
fallback x p = union p (return x)

many :: Parser a -> Parser [a]
many p = do
    x   <- p
    xs  <- manyOrNone p
    return $ x:xs

manyOrNone :: Parser a -> Parser [a]
manyOrNone p = union
    (concatenate (:) p (manyOrNone p))
    (pure [])

space :: Parser Char
space = charOf " \n\r\t"

spaces :: Parser String
spaces = union (many space) (pure "")

string :: String -> Parser String
string = (foldr (concatenate (:)) (pure [])) . (map char)

charOf :: String -> Parser Char
charOf = (foldr1 union) . (map char)

char :: Char -> Parser Char
char c = Parser (P.char c)

number :: Parser Int
number = digitsToNumber <$> many digit
    where
        digitsToNumber = foldl1 (\n d -> n * 10 + d)

digit :: Parser Int
digit = Parser P.digit

-- functions for bridging between Parser and ProtoParser:

lift :: (P.Parser a -> P.Parser b) -> Parser a -> Parser b
lift f p = Parser (f (extract p))

lift2 :: (P.Parser a -> P.Parser b -> P.Parser c) -> Parser a -> Parser b -> Parser c
lift2 f p q = Parser $ f (extract p) (extract q)

dropF :: (a -> Parser b) -> (a -> P.Parser b)
dropF f x = extract $ f x

extract :: Parser a -> P.Parser a
extract (Parser p) = p