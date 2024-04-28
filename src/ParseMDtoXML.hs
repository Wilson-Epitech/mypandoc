{--
-- MYPANDOC PROJECT, 2024
-- mypandoc
-- File description:
-- ParseMDtoXML
--}

module ParseMDtoXML (parseMDtoXML) where

import Data.List (isPrefixOf, intercalate)
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)

clearString :: String -> String
clearString [] = []
clearString (':':xs) = clearString xs
clearString ('=':xs) = clearString xs
clearString (' ':xs) = clearString xs
clearString ('#':xs) = clearString xs
clearString str = str

parseString :: String -> String -> Maybe String
parseString [] str = Just (clearString str)
parseString _ [] = Nothing
parseString substr str
    | substr `isPrefixOf` str = Just (clearString (drop (length substr) str))
    | otherwise = Nothing

writeCodeBlock :: Int -> [String] -> [String]
writeCodeBlock 0 xs = "<codeblock>" : parseBody 1 xs
writeCodeBlock 1 xs = "</codeblock>" : parseBody 0 xs
writeCodeBlock _ xs = xs

handleList :: Int -> String -> [String] -> [String]
handleList 0 x xs = "<list>" : ("<paragraph>" ++ fromMaybe ""
    (parseString "-" x) ++ "</paragraph>") : parseBody 1 xs
handleList count x xs = ("<paragraph>" ++ fromMaybe "" 
    (parseString "-" x) ++ "</paragraph>") : parseBody (count + 1) xs

parseBody :: Int -> [String] -> [String]
parseBody _ [] = []
parseBody count (x:xs)
    | "```" `isPrefixOf` x = writeCodeBlock count xs
    | "-" `isPrefixOf` x = handleList count x xs
    | count > 0 = ("</list>" ++ x) : parseBody 0 xs
    | otherwise = x : parseHeader count xs

writeHeader :: Int -> [String] -> [String]
writeHeader 0 xs = parseHeader 1 xs
writeHeader 1 xs = ("    </header>" ++ "\n    <body>") : parseHeader 0 xs
writeHeader _ xs = xs

parseHeader :: Int -> [String] -> [String]
parseHeader _ [] = []
parseHeader count (x:xs)
    | null x = parseHeader count xs
    | "---" `isPrefixOf` x = writeHeader count xs
    | "title:" `isPrefixOf` x = ("    <header title=\"" ++ fromMaybe "" 
        (parseString "title:" x) ++ "\">") : parseHeader count xs
    | "author:" `isPrefixOf` x = ("        <author>" ++ fromMaybe ""
        (parseString "author:" x) ++ "</author>") : parseHeader count xs
    | "date:" `isPrefixOf` x = ("        <date>" ++ fromMaybe
        "" (parseString "date:" x) ++ "</date>") : parseHeader count xs
    | otherwise = parseBody count (x:xs)

parseDocument :: [String] -> [String]
parseDocument [] = []
parseDocument (x:xs) 
    | "---" `isPrefixOf` x = parseHeader 0 (x:xs)
    | otherwise = parseBody 0 (x:xs)

parseMDtoXML :: [String] -> [String]
parseMDtoXML file = "<document>" : parseDocument file ++ ["    </body>"] ++ ["</document>"]
