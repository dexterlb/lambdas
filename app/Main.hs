module Main where

import Procedures
import Lambdas
import qualified Parser as P
import qualified Data.HashMap.Strict as H

import Debug.Trace

data State = State
    { procedures :: Cache
    }

data Command = Define Definition | List

instance P.Parseable Command where
    parser = do
        P.spaces
        cmd <- P.unionl
            [ defineParser
            , listParser
            ]
        P.spaces
        return cmd

defineParser :: P.Parser Command
defineParser = do
    P.string "define"
    P.spaces
    command <- Define <$> P.parser
    return command

listParser :: P.Parser Command
listParser = do
    P.string "list"
    return List

initialState :: State
initialState = State { procedures = H.empty }

processCommand :: Command -> State -> (State, String)
processCommand List s = (s, showCache (procedures s))
processCommand (Define def) s
    | (Just pr) <- result = (State { procedures = pr }, "ok")
    | otherwise = (s, "fail")
    where result = define def (procedures s)

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
