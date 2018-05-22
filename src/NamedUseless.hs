module NamedUseless where

import NamedLambdas

substitute :: Expression -> Variable -> Expression -> Expression
substitute (Variable x) y n
    | x == y    = n
    | otherwise = Variable x

substitute (Application m n) x p = Application (substitute m x p) (substitute n x p)
substitute (Lambda x m) t to
    | x == t                    = Lambda x m
    | t `inAV` m && x `inFV` to = (substitute (bvrename (Lambda x m) x v) t to)
    | otherwise                 = Lambda x (substitute m t to)
    where
        v = head $ filter (not . isBad) (allVars)
        isBad z = z `inFV` to || z `inAV` m

bvrename :: Expression -> Variable -> Variable -> Expression
bvrename (Variable x) _ _ = (Variable x)
bvrename (Application m n) x y = Application (bvrename m x y) (bvrename n x y)
bvrename (Lambda z m) x y
    | z == x    = (Lambda y (substitute (bvrename m x y) x (Variable y)))
    | otherwise = (Lambda z (bvrename m x y))

inAV :: Variable -> Expression -> Bool
inAV x p = inFV x p || inBV x p

inFV :: Variable -> Expression -> Bool
inFV x (Variable z)
    | x == z    = True
    | otherwise = False
inFV x (Application m n) = inFV x m || inFV x n
inFV x (Lambda y m)
    | x == y    = False
    | otherwise = inFV x m

inBV :: Variable -> Expression -> Bool
inBV x (Variable _) = False
inBV x (Application m n) = inBV x m || inBV x n
inBV x (Lambda y m)
    | x == y    = True
    | otherwise = inBV x m

