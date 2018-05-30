module Main where

import Procedures
import Lambdas
import qualified Data.HashMap.Strict as H

data State = State
    { procedures :: Cache
    }

initialState :: State
initialState = State { procedures = H.empty }

prompt :: (State, String) -> (State, String)
prompt (state, s) = (state, "foo " ++ s)

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
