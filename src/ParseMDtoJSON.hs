{--
-- MYPANDOC PROJECT, 2024
-- mypandoc
-- File description:
-- ParseMDtoJSON
--}

module ParseMDtoJSON (parseMDtoJSON) where
import Data.Maybe (catMaybes)
import Data.List (isInfixOf, isPrefixOf, find)


--- lil functions ---

isInTab :: [String] -> Char -> Bool
isInTab linesList charToFind =
    any (\line -> not (null line) && elem charToFind line) linesList

getByChar :: [String] -> Char -> String
getByChar linesList charToFind =
    case find (\line -> not (null line) && head line == charToFind) linesList of
        Just line -> line
        Nothing -> ""

getWordBySearch :: String -> [String] -> Maybe String
getWordBySearch search x =
    case filter (isPrefixOf (search ++ ":")) x of
        [] -> Nothing
        (line:_) -> Just $ dropWhile (== ' ') (drop (length search + 1) line)

--- end lil functions ---

--- PArseMdtoJson ---

toTab :: [String] -> [String]
toTab file = finalOutput (headerPart file) file

parseMDtoJSON :: [String] -> [String]
parseMDtoJSON [] = []
parseMDtoJSON ("---" : rest) = toTab rest
parseMDtoJSON _ = []

finalOutput :: [String] -> [String] -> [String]
finalOutput beforeSection postSection = beforeSection


-- END HEADER PART --
