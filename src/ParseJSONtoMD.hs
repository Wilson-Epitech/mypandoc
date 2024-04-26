{--
-- MYPANDOC PROJECT, 2024
-- mypandoc
-- File description:
-- ParseXMLtoMD
--}

module ParseJSONtoMD (parseJSONtoMD) where
import Parser (parseString)

cleanString :: String -> String
cleanString [] = []
cleanString ('\"':xs) = init xs
cleanString str = str

isNeedClear :: String -> Bool
isNeedClear [] = False
isNeedClear [x]
    | x == '\"' = True
    | otherwise = False
isNeedClear (_:xs) = isNeedClear xs

--getTitle--
getTitle :: String -> String
getTitle str = "title: " ++ init (cleanString str)

--getAuthor--
getAuthor :: String -> String
getAuthor str = "author: " ++ init (cleanString str)

--getDate--
getDate :: String -> String
getDate str = "date: " ++ cleanString str

--parseJsonHeaderToMDHeader--
parseJsonHeaderToMDHeader :: [String] -> [String]
parseJsonHeaderToMDHeader ("},":xs) = "---":parseJson xs
parseJsonHeaderToMDHeader (x:xs) = case parseString "\"title\"" x of
        Just res -> getTitle res: parseJsonHeaderToMDHeader xs
        Nothing -> case parseString "\"author\"" x of
            Just res -> getAuthor res :parseJsonHeaderToMDHeader xs
            Nothing -> case parseString "\"date\"" x of
                    Nothing -> parseJsonHeaderToMDHeader xs
                    Just res -> getDate res:parseJsonHeaderToMDHeader xs
parseJsonHeaderToMDHeader [] = ["---"]

--parseCodeBlock--
parseCodeBlock :: [String] -> [String] -> [String]
parseCodeBlock ("]":_) tmp = mappend tmp ["```", ""]
parseCodeBlock ("],":xs) tmp = parseCodeBlock xs tmp 
parseCodeBlock ("[":x:xs) [] = parseCodeBlock xs ("```":[tail x])
parseCodeBlock ("[":x:xs) tmp = parseCodeBlock xs (tail x:tmp)
parseCodeBlock _  _ = ["```"]


--takeOffCodeBlock--
takeOffCodeBlock :: [String] -> [String]
takeOffCodeBlock ("]":xs) = xs
takeOffCodeBlock ("],":xs) = takeOffCodeBlock xs 
takeOffCodeBlock ("[":_:xs) = takeOffCodeBlock xs
takeOffCodeBlock tmp = tmp

getNewIndex :: [String] -> String
getNewIndex [] = ""
getNewIndex (x:_) = x

--getLink--
getLink :: [String] -> String -> String -> Bool -> String
getLink [] url content False = mappend (mappend (mappend 
    (mappend (mappend "[" (cleanString content)) "]") "(")
    (cleanString  url)) ")"
getLink [] url content True = mappend (mappend (mappend 
    (mappend (mappend "![" (cleanString content)) "]") "(")
    (cleanString  url)) ")"
getLink (x:xs) url content b = if url /= "" && content /= ""
    then getLink [] url content b
    else case parseString "\"url\": " x of
    Just res -> getLink xs (init res) content b
    Nothing -> case parseString "\"content\": [" x of
        Just _ -> getLink xs url (getNewIndex xs) b
        Nothing -> case parseString "\"alt\": [" x of
            Just _ -> getLink xs url (getNewIndex xs) b
            Nothing -> getLink xs url content b
        --getLink xs url content fait le en pattern matching ptn

--goToNextScope--
goToNextScope :: [String] -> [String]
goToNextScope ("}":xs) = xs
goToNextScope (_:xs) = goToNextScope xs
goToNextScope _ = []

--parseList--
parseList :: [String] -> [String]
parseList [] = [""]
parseList ("],": xs) = parseList xs
parseList ("[":x:xs) = ("- " ++ tail x):parseList xs
parseList _ = [""]

--parseTheString--
parseTheString :: String -> String -> Maybe String
parseTheString [] str = Just str
parseTheString _ [] = Nothing
parseTheString (x:xs) (y:ys)
    | x == y = parseTheString xs ys
    | otherwise = parseTheString (x:xs) ys

--checkOtherArray--
checkOtherArray :: Int -> [String] -> [String]
checkOtherArray i [] = parseArray i []
checkOtherArray i (x:xs) = case parseString "\"link\"" x of
    Just _ -> "|":getLink xs "" "" False:parseArray i (goToNextScope xs)
    Nothing -> case parseString "\"image\"" x of
        Just _ -> "|":getLink xs "" "" True: parseArray i (goToNextScope xs)
        Nothing -> case parseTheString "\"" x of
            Just res -> "|":init res:parseArray i xs
            Nothing -> parseArray i xs

--parseArray--
parseArray :: Int -> [String] -> [String]
parseArray _ [] = []
parseArray i ("[":x:xs) = cleanString x:parseArray i xs
parseArray i ("],":xs) = parseArray i xs
parseArray i ("]":xs) = parseArray i xs
parseArray i ("{":xs) = parseArray i xs
parseArray i ("},":xs) = parseArray i xs
parseArray i ("\"codeblock\": [":xs) =
    mappend (parseCodeBlock xs []) (parseArray i (takeOffCodeBlock xs))
parseArray i ("\"list\": [":xs) = 
    mappend (parseList xs) (parseArray i (takeOffCodeBlock xs))
parseArray i (x:xs) = case parseString "\"section\": {" x of
    Just _ -> "":"":parseSection (i + 1) xs
    Nothing -> case parseString "\"bold\": " x of
        Just res -> "|":("**" ++ cleanString res ++ "**"):parseArray i xs
        Nothing -> case parseString "\"italic\": " x of
            Just res -> "|":("*" ++ cleanString res ++ "*"):parseArray i xs
            Nothing -> case parseString "\"code\"" x of
                Just res -> "|":("`" ++ cleanString res ++ "`"):parseArray i xs
                Nothing -> checkOtherArray i (x:xs)


--parseSection--
parseSection :: Int -> [String] -> [String]
parseSection i (x:xs) = case parseString "\"title\"" x of
    Just res -> if res /= "\"\"," 
        then (replicate i '#' ++ " " ++ cleanString res):"":parseSection i xs
    else parseSection i xs
    Nothing -> case parseString "\"content\"" x of
        Just _ -> parseArray i xs
        Nothing -> case parseString "\"section\"" x of
            Just _ -> "":parseSection (i + 1) xs
            Nothing -> []
parseSection _ [] = []

--parseJsonBodyToMDBody--
parseJsonBodyToMDBody :: [String] -> [String]
parseJsonBodyToMDBody ("[":x:xs) = cleanString x:parseArray 0 xs
parseJsonBodyToMDBody (x:xs) = case parseString "\"section\""x of
    Just _ -> parseSection 0 xs
    Nothing -> case parseString "[" x of
        Just _ -> x:parseArray 0 xs
        Nothing -> []
parseJsonBodyToMDBody [] = []

--parseJson--
parseJson :: [String] -> [String]
parseJson [] = []
parseJson ("\"header\": {":xs) = "---":parseJsonHeaderToMDHeader xs
parseJson ("\"body\": [":xs) = "":parseJsonBodyToMDBody xs
parseJson _ = []

appendToLast :: [String] -> [String]
appendToLast [] = []
appendToLast (x:"|":y:xs) = appendToLast ((x ++ y):xs)
appendToLast (x:xs) = x:appendToLast xs

upgradeTab :: [String] -> [String]
upgradeTab [] = []
upgradeTab (x:xs) = if isNeedClear x
    then init x:upgradeTab xs else x:upgradeTab xs

--parseJSONtoMD --
parseJSONtoMD :: [String] -> [String]
parseJSONtoMD ("{":x:xs) = appendToLast (upgradeTab (parseJson (x:xs)))
parseJSONtoMD _ = []
