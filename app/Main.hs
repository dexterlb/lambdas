module Main where

import Procedures
import Lambdas
import Maths
import qualified Parser as P
import qualified Data.HashMap.Strict as H

import qualified NamedLambdas as NL
import qualified NamelessLambdas as UL

import Cache (Cache)
import qualified Cache as C

import Data.Maybe (fromJust)
import Data.List (intercalate)

import Debug.Trace

data State = State
    { procedures :: Cache
    , reductionLimit :: Int
    }

data Command
    = Define Definition
    | List
    | See Expression
    | Do Expression

instance P.Parseable Command where
    parser = do
        P.spaces
        cmd <- P.unionl
            [ defineParser
            , listParser
            , seeParser
            , doParser
            ]
        P.spaces
        return cmd

defineParser :: P.Parser Command
defineParser = do
    P.string "define"
    P.spaces
    definition <- P.parser
    return $ Define definition

listParser :: P.Parser Command
listParser = do
    P.string "list"
    return List

seeParser :: P.Parser Command
seeParser = do
    P.string "see"
    P.spaces
    term <- P.parser
    return $ See term

doParser :: P.Parser Command
doParser = do
    P.string "do"
    P.spaces
    term <- P.parser
    return $ Do term

initialState :: State
initialState = State
    { procedures = C.empty
    , reductionLimit = 1000
    }

processCommand :: Command -> State -> (State, String)

processCommand List s = (s, showCache (procedures s))

processCommand (Define def) s
    | (Just (pr, Ok))       <- result = (s { procedures = pr }, "ok")
    | (Just (pr, GiveUp))   <- result = (s { procedures = pr }, "ok, but gave up on reduction")
    | otherwise = (s, "fail")
    where result = defineReduce (reductionLimit s) def (procedures s)

processCommand (See term) s
    | (Just namedTerm) <- result = (s, show namedTerm)
    | otherwise                  = (s, "replace error")
    where
        result = name vars =<< toUnnamed vars (procedures s) term
        vars = fvList term

processCommand (Do term) s
    | (Just (namedTerm, Ok)) <- result      = (s, show namedTerm)
    | (Just (namedTerm, GiveUp)) <- result  = (s, "[gave up] " ++ (show namedTerm))
    | otherwise                             = (s, "replace error")
    where
        result = do
            term <- toUnnamed vars (procedures s) term
            let (reduced, finished) = reduceUntil (reductionLimit s) term
            namedTerm <- fromUnnamed (procedures s) vars reduced
            return (namedTerm, finished)
        vars = fvList term



prompt :: (State, String) -> (State, String)
prompt (state, s)
    | (Just cmd) <- (P.parse s) = processCommand cmd state
    | otherwise                 = (state, "parse error")

mainLoop :: [String] -> [String]
mainLoop = mainLoop' initialState

mainLoop' _ [] = []
mainLoop' state (x:xs) = result : (mainLoop' newState xs)
    where
        (newState, result) = prompt (state, x)


dialog :: String -> String
dialog = unlines . mainLoop . lines

main :: IO ()
main = interact dialog
