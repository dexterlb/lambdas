module Procedures where

import qualified NamedLambdas as NL
import qualified NamelessLambdas as UL
import Lambdas
import Data.List (intercalate)
import qualified Parser as P
import Parser (Parser)

data Call = Call String Context

instance Show Call where
    show (Call f vars) = f ++ "(" ++ (intercalate ", " vars) ++ ")"

data Procedure = Procedure Call NL.Expression

instance Show Procedure where
    show (Procedure call body)
        = (show call) ++ " = " ++ (show body)

instance P.Parseable Call where
    parser = do
        P.spaces
        fh <- P.charOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        ft <- P.manyOrNone $ P.charOf "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890_"
        let f = fh : ft

        P.spaces
        P.char '('
        P.spaces
        varsh <- NL.varParser
        varst <- P.manyOrNone $ do
            P.spaces
            P.char ','
            P.spaces
            var <- NL.varParser
            P.spaces
            return var

        let vars = varsh : varst

        P.char ')'

        return $ Call f vars