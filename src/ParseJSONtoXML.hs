{--
-- MYPANDOC PROJECT, 2024
-- mypandoc
-- File description:
-- ParseJSONtoXML
--}

module ParseJSONtoXML (parseJSONtoXML) where

import Parser (parseString)

cleanString :: String -> String
cleanString [] = []
cleanString ('\"':xs) = init xs
cleanString str = str

isNeedClear :: String -> Bool
isNeedClear [] = False
isNeedClear [x]
    | x == '\"' = True
    | x == ',' = True
    | otherwise = False
isNeedClear (_:xs) = isNeedClear xs

--getTitle--
getTitle :: String -> String
getTitle str = "title=" ++ init str ++ ">"

--getAuthor--
getAuthor :: String -> String
getAuthor str = "<author>" ++ init (cleanString str) ++ "</author>"

--getDate--
getDate :: String -> String
getDate str = "<date>" ++ cleanString str ++ "</date>"

--parseJsonHeaderToXMLHeader--
parseJsonHeaderToXMLHeader :: [String] -> [String]
parseJsonHeaderToXMLHeader ("},":xs) = "</header>":parseJson xs
parseJsonHeaderToXMLHeader (x:xs) = case parseString "\"title\"" x of
        Just res -> getTitle res: parseJsonHeaderToXMLHeader xs
        Nothing -> case parseString "\"author\"" x of
            Just res -> getAuthor res :parseJsonHeaderToXMLHeader xs
            Nothing -> case parseString "\"date\"" x of
                    Nothing -> parseJsonHeaderToXMLHeader xs
                    Just res -> getDate res:parseJsonHeaderToXMLHeader xs
parseJsonHeaderToXMLHeader [] = ["</header>"]

--parseCodeBlock--
parseCodeBlock :: [String] -> [String] -> [String]
parseCodeBlock ("]":_) tmp = mappend tmp ["</codeblock>", ""]
parseCodeBlock ("],":xs) tmp = parseCodeBlock xs tmp
parseCodeBlock ("[":x:xs) [] = 
    parseCodeBlock xs 
    ("<codeblock>":["<paragraph>" ++ cleanString x ++ "</paragraph>"])
parseCodeBlock ("[":x:xs) tmp =
    parseCodeBlock xs 
    (("<paragraph>" ++ cleanString x ++ "</paragraph>"):tmp)
parseCodeBlock _  _ = ["</codeblock>"]


--takeOffCodeBlock--
takeOffCodeBlock :: [String] -> [String]
takeOffCodeBlock ("]":xs) = xs
takeOffCodeBlock ("],":xs) = takeOffCodeBlock xs
takeOffCodeBlock ("[":_:_:xs) = takeOffCodeBlock xs
takeOffCodeBlock tmp = tmp

getNewIndex :: [String] -> String
getNewIndex [] = ""
getNewIndex (x:_) = x

--getLink--
getLink :: [String] -> String -> String -> Bool -> String
getLink [] url content False = mappend (mappend (mappend
    (mappend "<link url=" url) ">")
    (cleanString content)) "</link>"
getLink [] url content True = mappend (mappend (mappend
    (mappend "<image url=" url) ">")
    (cleanString content)) "</image>"
getLink (x:xs) url content b = if url /= "" && content /= ""
    then getLink [] url content b
    else case parseString "\"url\": " x of
    Just res -> getLink xs (init res) content b
    Nothing -> case parseString "\"content\": [" x of
        Just _ -> getLink xs url (getNewIndex xs) b
        Nothing -> case parseString "\"alt\": [" x of
            Just _ -> getLink xs url (getNewIndex xs) b
            Nothing -> getLink xs url content b

--goToNextScope--
goToNextScope :: [String] -> [String]
goToNextScope ("}":xs) = xs
goToNextScope (_:xs) = goToNextScope xs
goToNextScope _ = []

--parseList--
parseList :: [String] -> [String]
parseList [] = [""]
parseList ("],": xs) = "|":"</paragraph>":parseList xs
parseList ("]":_) = ["|", "</paragraph>"]
parseList ("[":x:xs) = ("<paragraph>" ++ tail (init x)):parseList xs
parseList _ = [""]

--checkOtherArray--
checkOtherArray :: Int -> [String] -> [String]
checkOtherArray i [] = parseArray i []
checkOtherArray i (x:xs) = case parseString "\"code\"" x of
    Just res ->
            "|":("<code>" ++ cleanString res ++ "</code>"):parseArray i xs
    Nothing -> case parseString "\"link\"" x of
        Just _ -> "|":getLink xs "" "" False:parseArray i (goToNextScope xs)
        Nothing -> case parseString "\"image\"" x of
            Just _ -> "|":getLink xs "" "" True:parseArray i (goToNextScope xs)
            Nothing -> case parseTheString "\"" x of
                Just res -> "|":init res:parseArray i xs
                Nothing -> parseArray i xs

checkForStr :: String -> String
checkForStr str
    | isNeedClear str = init str
    | otherwise = str

--parseTheString--
parseTheString :: String -> String -> Maybe String
parseTheString [] str = Just str
parseTheString _ [] = Nothing
parseTheString (x:xs) (y:ys)
    | x == y = parseTheString xs ys
    | otherwise = parseTheString (x:xs) ys

--parseArray--
parseArray :: Int -> [String] -> [String]
parseArray _ [] = ["</body>"]
parseArray i ("[":x:xs) =
    ("<paragraph>" ++ checkForStr (cleanString x)):parseArray i xs
parseArray 0 ("]":"}":xs) = parseArray 0 xs
parseArray i ("]":"}":xs) = "</section>":parseArray (i - 1) xs
parseArray i ("],":xs) = "|":"</paragraph>":parseArray i xs
parseArray i ("]":xs) = "|":"</paragraph>":parseArray i xs
parseArray i ("{":xs) = parseArray i xs
parseArray i ("},":xs) = parseArray i xs
parseArray i ("\"codeblock\": [":xs) =
    mappend (parseCodeBlock xs []) (parseArray i (takeOffCodeBlock xs))
parseArray i ("\"list\": [":xs) =
    mappend (mappend ("<list>":parseList xs)
    ["</list>"]) (parseArray i (takeOffCodeBlock xs))
parseArray i (x:xs) = case parseString "\"section\": {" x of
    Just _ -> "":parseSection (i + 1) xs
    Nothing -> case parseString "\"bold\": " x of
        Just res -> 
            "|":("<bold>" ++ cleanString res ++ "</bold>"):parseArray i xs
        Nothing -> case parseString "\"italic\": " x of
            Just res -> 
                "|":("<italic>" ++ cleanString res ++ "</italic>"):
                parseArray i xs
            Nothing -> checkOtherArray i (x:xs)

--parseSection--
parseSection :: Int -> [String] -> [String]
parseSection i (x:xs) = case parseString "\"title\"" x of
    Just res -> ("<section title=" ++ init res ++ ">"):"":parseSection i xs
    Nothing -> case parseString "\"content\"" x of
        Just _ -> parseArray i xs
        Nothing -> case parseString "\"section\"" x of
            Just _ -> "":parseSection (i + 1) xs
            Nothing -> []
parseSection _ [] = []

--parseJsonBodyToXMLBody--
parseJsonBodyToXMLBody :: [String] -> [String]
parseJsonBodyToXMLBody ("[":x:xs) = 
    ("<paragraph>" ++ cleanString x):parseArray 0 xs
parseJsonBodyToXMLBody (x:xs) = case parseString "\"section\"" x of
    Just _ -> parseSection 0 xs
    Nothing -> case parseString "[" x of
        Just _ -> x:parseArray 0 xs
        Nothing -> []
parseJsonBodyToXMLBody [] = []

--parseJson--
parseJson :: [String] -> [String]
parseJson [] = []
parseJson ("\"header\": {":xs) = "<header ":"|":parseJsonHeaderToXMLHeader xs
parseJson ("\"body\": [":xs) = "<body>":parseJsonBodyToXMLBody xs
parseJson _ = []

appendToLast :: [String] -> [String]
appendToLast [] = ["</document>"]
appendToLast (x:"|":y:xs) = appendToLast ((x ++ y):xs)
appendToLast (x:xs) = x:appendToLast xs

upgradeTab :: [String] -> [String]
upgradeTab [] = []
upgradeTab (x:xs) = if isNeedClear x
    then init x:upgradeTab xs else x:upgradeTab xs

-- parseJSONtoXML --
parseJSONtoXML :: [String] -> [String]
parseJSONtoXML ("{":x:xs) = "<document>":appendToLast 
    (upgradeTab (parseJson (x:xs)))
parseJSONtoXML _ = []
