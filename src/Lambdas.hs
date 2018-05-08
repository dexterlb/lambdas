module Lambdas where

import qualified NamedLambdas as NL
import qualified NamelessLambdas as UL

import Data.List (elemIndex)
import Control.Monad (liftM2)

type Context = [NL.Variable]

-- substitute :: UL.Expression -> Int -> UL.Expression -> UL.Expression

unname :: Context -> NL.Expression -> Maybe UL.Expression
unname c (NL.Variable x)      =        UL.Variable <$> index c x
unname c (NL.Application m n) = liftM2 UL.Application  (unname c m) (unname c n)
unname c (NL.Lambda x m)      =        UL.Lambda   <$> unname (push x c) m

name :: Context -> UL.Expression -> Maybe NL.Expression
name c e = name' c e (varsWithout c)

name' :: Context -> UL.Expression -> [NL.Variable] -> Maybe NL.Expression
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
