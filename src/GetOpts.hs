{--
-- MYPANDOC PROJECT, 2024
-- mypandoc
-- File description:
-- GetOpts
--}

module GetOpts (getOpts) where

import Configuration (Format(..), Conf, ifile, oformat, ofile, iformat)

validOpts :: Conf -> Maybe Conf
validOpts conf | ifile conf == Nothing || oformat conf == Nothing = Nothing
               | otherwise = Just conf

getFormatInput :: [String] -> Maybe Format
getFormatInput (('-':ab):xs) = Just ("Markdown")
getFormatInput (('<':ab):xs) = Just ("XML")
getFormatInput (('{':ab):xs) = Just ("Json")

getFormat :: String -> Maybe Format
getFormat "xml" = Just XML
getFormat "json" = Just JSON
getFormat "markdown" = Just MARKDOWN
getFormat _ = Nothing

getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf [] = validOpts conf
getOpts conf ("-i" : v : r) = getOpts (conf {ifile = (Just v)}) r
getOpts conf ("-f" : v : r) = getOpts (conf {oformat = (getFormat v)}) r
getOpts conf ("-o" : v : r) = getOpts (conf {ofile = (Just v)}) r
getOpts conf ("-e" : v : r) = getOpts (conf {iformat = (getFormat v)}) r
getOpts _ _ = Nothing
