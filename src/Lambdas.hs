module Lambdas where

import qualified NamedLambdas as NL
import qualified NamelessLambdas as UL
import Maths

import Data.List (elemIndex)
import Control.Monad (liftM2)
import Data.Maybe (fromJust)

type Context = [NL.Variable]

reduceUntil :: Int -> UL.Term -> (UL.Term, ProcessResult)
reduceUntil = limitedFixedPoint reduce

reduce :: UL.Term -> UL.Term
reduce (UL.Application (UL.Lambda m) n) = up (substitute m 0 (up n 1)) (-1)
reduce (UL.Application m n)             = UL.Application (reduce m) (reduce n)
reduce (UL.Lambda m)                    = UL.Lambda      (reduce m)
reduce (UL.Variable x)                  = UL.Variable    x

substitute :: UL.Term -> Int -> UL.Term -> UL.Term
substitute (UL.Variable x) y to
    | x == y = to
    | x /= y = (UL.Variable x)
substitute (UL.Application m n) y to = UL.Application (substitute m y to) (substitute n y to)
substitute (UL.Lambda m) y to = UL.Lambda (substitute m (y + 1) (up to 1))

up :: UL.Term -> Int -> UL.Term
up to n = up' to n 0

up' :: UL.Term -> Int -> Int -> UL.Term
up' (UL.Variable x) d c
    | x >= c = (UL.Variable (x + d))
    | otherwise = (UL.Variable x)
up' (UL.Application m n) d c = UL.Application (up' m d c) (up' n d c)
up' (UL.Lambda m) d c = UL.Lambda (up' m d (c + 1))

onNamed :: (UL.Term -> UL.Term) -> NL.Term -> NL.Term
onNamed f m = fromJust $ do
    let vars = NL.fvList m
    m'       <- unname vars m
    let n'   = f m'
    n        <- name vars n'
    return n

unname :: Context -> NL.Term -> Maybe UL.Term
unname c (NL.Variable x)      =        UL.Variable <$> index c x
unname c (NL.Application m n) = liftM2 UL.Application  (unname c m) (unname c n)
unname c (NL.Lambda x m)      =        UL.Lambda   <$> unname (push x c) m

name :: Context -> UL.Term -> Maybe NL.Term
name c e = name' c e (varsWithout c)

name' :: Context -> UL.Term -> [NL.Variable] -> Maybe NL.Term
name' c (UL.Variable i) _         = NL.Variable <$> at i c
name' c (UL.Application m n) vars = liftM2 NL.Application (name' c m vars) (name' c n vars)
name' c (UL.Lambda m) vars        = (NL.Lambda x) <$> (name' (push x c) m freshVars)
    where
        (x:freshVars) = vars

index :: Context -> NL.Variable -> Maybe Int
index c x = elemIndex x c

at :: Int -> Context -> Maybe NL.Variable
at i c
    | length c > i = Just $ c !! i
    | otherwise    = Nothing

push :: NL.Variable -> Context -> Context
push x c = x : c

varsWithout :: Context -> [NL.Variable]
varsWithout c = filter (\x -> index c x == Nothing) NL.allVars
