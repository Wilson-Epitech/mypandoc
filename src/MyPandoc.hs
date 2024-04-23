{--
-- MYPANDOC PROJECT, 2024
-- myPandoc
-- File description:
-- pandoc proj
--}

module MyPandoc (myPandoc) where

import Configuration (Conf(..), Format(..))
import System.Exit (exitSuccess, exitWith, ExitCode(ExitFailure))

import Data.Maybe (fromJust)

import ParseJSONtoMD (parseJSONtoMD)
import ParseJSONtoXML (parseJSONtoXML)
import ParseMDtoJSON (parseMDtoJSON)
import ParseMDtoXML (parseMDtoXML)
import ParseXMLtoJSON (parseXMLtoJSON)
import ParseXMLtoMD (parseXMLtoMD)

-- Lines --
getFormatInput :: [String] -> Maybe Format
getFormatInput (('<' : _) : _) = Just XML
getFormatInput (('{' : _) : _) = Just JSON
getFormatInput (('-' : _) : _) = Just MD
getFormatInput _ = Nothing

-- Output File -- Lines -- 
printLines :: Maybe String -> [String] -> IO ()
printLines _ [] = exitSuccess
printLines Nothing (x : xs) = putStrLn x >> printLines Nothing xs
printLines _ _ = exitWith (ExitFailure 84)

-- runPandoc -- Conf -- Lines --
runPandoc :: Conf -> [String] -> [String]
runPandoc (Conf _ (Just XML) _ (Just JSON)) file = parseXMLtoJSON file
runPandoc (Conf _ (Just XML) _ (Just MD)) file = parseXMLtoMD file
runPandoc (Conf _ (Just JSON) _ (Just XML)) file = parseJSONtoXML file
runPandoc (Conf _ (Just JSON) _ (Just MD)) file = parseJSONtoMD file
runPandoc (Conf _ (Just MD) _ (Just XML)) file = parseMDtoXML file
runPandoc (Conf _ (Just MD) _ (Just JSON)) file = parseMDtoJSON file
runPandoc _ _ = []

-- getInputConf -- Conf -- Lines --
getInputConf :: Conf -> [String] -> Conf
getInputConf (Conf a b c Nothing) file = (Conf a b c (getFormatInput file))
getInputConf conf _ = conf

-- Configuration --
myPandoc :: Conf -> IO ()
myPandoc conf@(Conf a _ c _) = do
    file <- readFile (fromJust a)
    printLines c (runPandoc (getInputConf conf (lines file)) (lines file))
