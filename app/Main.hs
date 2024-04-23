{--
-- MYPANDOC PROJECT, 2024
-- mypandoc
-- File description:
-- Main
--}

module Main (main) where

import GetOpts (getOpts)
import MyPandoc (myPandoc)
import Configuration (defaultConf)
import InvalidArguments (invalidArguments)

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case getOpts defaultConf args of
        Nothing -> invalidArguments
        Just conf -> myPandoc conf
