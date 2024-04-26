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

fillUntilFound :: [String] -> String -> [String]
fillUntilFound [] _ = []
fillUntilFound (x:xs) search | x == search = []
                              | otherwise = x : fillUntilFound xs search

skipUntilFound :: [String] -> String -> [String]
skipUntilFound [] _ = []
skipUntilFound (x:xs) search | x == search = xs
                              | otherwise = skipUntilFound xs search

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

-- word array

splitListOnSubstring :: String -> [String] -> [String]
splitListOnSubstring _ [] = []
splitListOnSubstring subStr (x:xs)
    | isInfixOf subStr x =
        let (before, after) = breakSubstring subStr x
            rest = drop (length subStr) after
        in before : if null rest then filteredResult else rest : filteredResult
    | otherwise = x : filteredResult
  where
    filteredResult = filter (not . null) (splitListOnSubstring subStr xs)

breakSubstring :: String -> String -> (String, String)
breakSubstring _ [] = ("", "")
breakSubstring subStr str = go [] str
  where
    go acc [] = (reverse acc, "")
    go acc s@(x:xs)
        | subStr `isPrefixOf` s = (reverse acc, s)
        | otherwise = go (x:acc) xs


 -- end word_arry


--- PArseMdtoJson ---

subGetuntilSec :: [String] -> [String]
subGetuntilSec file = skipUntilFound file (getByChar file '\n')


getHeaderPart :: [String] -> [String]
getHeaderPart parse = fillUntilFound parse (getByChar parse '-')


noSection :: [String] -> [String]
noSection postHead = skipUntilFound postHead (getByChar postHead '\n')

getUntilSection :: [String] -> [String]
getUntilSection postHead =
    fillUntilFound (subGetuntilSec postHead) (getByChar postHead '#')

--- COREEE ---

headerPart :: [String] -> [String]
headerPart file =
        if isInTab file '#' then
            parseHeaderMd (getHeaderPart file) (getUntilSection file)
        else
            parseHeaderMd (getHeaderPart file) (noSection file)

toTab :: [String] -> [String]
toTab file = finalOutput (headerPart file) file

parseMDtoJSON :: [String] -> [String]
parseMDtoJSON [] = []
parseMDtoJSON ("---" : rest) = toTab rest
parseMDtoJSON _ = []

finalOutput :: [String] -> [String] -> [String]
finalOutput beforeSection postSection = beforeSection

--- HeaderPart ---

parseHeaderMd :: [String] -> [String] -> [String]
parseHeaderMd header beforeSection =
    let titleValue = getWordBySearch "title" header
        authorValue = getWordBySearch "author" header
        dateValue = getWordBySearch "date" header
        jsonLines = formatHeader titleValue authorValue dateValue beforeSection
    in jsonLines

formatHeader :: Maybe String -> Maybe String -> Maybe String -> [String] -> [String]
formatHeader title author date beforeSection
    | null beforeSection = onlyHeader title author date
    | otherwise = (headerWithText title author date beforeSection)

onlyHeader :: Maybe String -> Maybe String -> Maybe String -> [String]
onlyHeader title Nothing Nothing =
    let headerLines = catMaybes
            [ Just "  \"header\": {"
            , fmap (\v -> "    \"title\": \"" ++ v ++ "\"") title
            ]
    in ["{"] ++ headerLines ++ ["  }"] ++ noBodyFormat

onlyHeader title author Nothing =
    let headerLines = catMaybes
            [ Just "  \"header\": {"
            , fmap (\v -> "    \"title\": \"" ++ v ++ "\",") title
            , fmap (\v -> "    \"author\": \"" ++ v ++ "\"") author
            ]
    in ["{"] ++ headerLines ++ ["  }"] ++ noBodyFormat

onlyHeader title author date =
    let headerLines = catMaybes
            [ Just "  \"header\": {"
            , fmap (\v -> "    \"title\": \"" ++ v ++ "\",") title
            , fmap (\v -> "    \"author\": \"" ++ v ++ "\",") author
            , fmap (\v -> "    \"date\": \"" ++ v ++ "\"") date
            ]
    in ["{"] ++ headerLines ++ ["  }"] ++ noBodyFormat


headerWithText :: Maybe String -> Maybe String -> Maybe String -> [String] -> [String]
headerWithText title Nothing Nothing bodyValue =
    let headerLines = catMaybes
            [ Just "  \"header\": {"
            , fmap (\v -> "    \"title\": \"" ++ v ++ "\"") title
            ]
    in ["{"] ++ headerLines ++ ["  }"]  ++
    bodyUntilSection (splitListOnSubstring "." bodyValue)

headerWithText title author Nothing bodyValue =
    let headerLines = catMaybes
            [ Just "  \"header\": {"
            , fmap (\v -> "    \"title\": \"" ++ v ++ "\",") title
            , fmap (\v -> "    \"author\": \"" ++ v ++ "\"") author
            ]
    in ["{"] ++ headerLines ++ ["  }"]  ++
    bodyUntilSection (splitListOnSubstring "." bodyValue)

headerWithText title author date bodyValue =
    let headerLines = catMaybes
            [ Just "  \"header\": {"
            , fmap (\v -> "    \"title\": \"" ++ v ++ "\",") title
            , fmap (\v -> "    \"author\": \"" ++ v ++ "\",") author
            , fmap (\v -> "    \"date\": \"" ++ v ++ "\"") date
            ]
    in ["{"] ++ headerLines ++ ["  }"]  ++
    bodyUntilSection (splitListOnSubstring "." bodyValue)


--- End Header Part ---


--- Body Part ---

noBodyFormat :: [String]
noBodyFormat = ["  \"body\": []\n}"]

bodyUntilSection :: [String] -> [String]
bodyUntilSection [] = []
bodyUntilSection (x:xs) =
    ["  \"body\": ["] ++
    formatElement x : map (\str -> "      [\n\t  \"" ++ str ++
    ".\"\n      ],") xs

formatElement :: String -> String
formatElement "" = ""
formatElement str = "      [\n\t  \"" ++ str ++ ".\"\n      ],"


-- END HEADER PART --
