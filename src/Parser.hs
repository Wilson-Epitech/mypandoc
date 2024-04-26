{-
-- MYPANDOC PROJECT, 2024
-- mypandoc
-- File description:
-- Parser
-}

module Parser (parseChar,
    parseAnyChar,
    parseOr,
    parseAnd,
    parseAndWith,
    parseMany,
    parseSome,
    parseUInt,
    parseInt,
    parseTuple,
    runParser,
    clearTab,
    parseString,
    parserString,
    Parser) where

import Control.Applicative (Alternative (empty), (<|>))

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

instance Functor Parser where
    fmap fct parser = Parser $ \str ->
        case runParser parser str of
            Left _ -> Left "none"
            Right (x, rest) -> Right (fct x, rest)


instance Applicative Parser where
    pure c = Parser (\str -> Right (c, str))
    (Parser ft) <*> (Parser ft2) = Parser $ \str ->
        case ft str of
            Left _ -> Left "none"
            Right (y, rest) -> case ft2 rest of
                Left _ -> Left "none"
                Right (z, rest2) -> Right (y z, rest2)

instance Alternative Parser where
    empty = Parser $ \_ -> Left "none"
    Parser x <|> Parser y = Parser $ \str ->
        either (\_ -> y str) Right (x str)

instance Monad Parser where
    return = pure
    (>>=) parser f = Parser $ \str ->
        case runParser parser str of
            Left err -> Left err
            Right (res, rest) -> runParser (f res) rest

parseChar :: Char -> Parser Char
parseChar c = Parser (\ (x:xs) -> if c == x then Right (c, xs) else Left xs)

parseAnyChar :: String -> Parser Char
parseAnyChar str = Parser tmp
    where
        tmp [] = Left "Empty String"
        tmp (x:xs)
            | x `elem` str = Right (x, xs)
            | otherwise = Left "none"

parseOr :: Parser a -> Parser a -> Parser a
parseOr ft ft2 = ft <|> ft2

-- a la place du ft <|> ft2 => Parser $ \str ->
--    case runParser ft str of
--        Right result -> Right result
--        Left _ -> runParser ft2 str


parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd ft ft2 = Parser $ \str -> case runParser ft str of
    Left _ -> Left "none"
    Right (res, rest) -> case runParser ft2 rest of
        Left _ -> Left "none"
        Right (res2, rest2) -> Right ((res, res2), rest2)


parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith ft ft2 ft3 = ft <$> ft2 <*> ft3

-- a la plce du f <$> ft2 <*> ft3 = Parser $ \str -> case runParser ft2 str of
-- Left _ -> Left "none"
-- Right (res, rest) -> case runParser ft3 rest of
    -- Left _ -> Left "none"
    -- Right (res2, rest2) -> Right (ft res res2, rest2)


parseMany :: Parser a -> Parser [a]
parseMany ft = Parser $ \str -> case runParser ft str of
    Left _ -> Right ([], str)
    Right (res, rest) -> case runParser (parseMany ft) rest of
        Left _ -> Left "none"
        Right (res1, rest1) -> Right (res : res1, rest1)

parseSome :: Parser a -> Parser [a]
parseSome ft = Parser $ \str -> case runParser ft str of
    Right (res, rest) -> case runParser (parseMany ft) rest of
        Right (res1, rest1) -> Right (res:res1, rest1)
        Left _ -> Right ([res], rest)
    Left _ -> Left "none"

parseUInt :: Parser Int
parseUInt = Parser $ \str -> case reads str of
    [(n, rest)] | n > 0 -> Right (n, rest)
    _ -> Left "none"

parseInt :: Parser Int
parseInt = Parser $ \str -> case str of
    "" -> Left "Empty string"
    ('-':xs) -> case runParser parseUInt xs of
        Right (n, rest) -> Right (-n, rest)
        Left _ -> Left "none"
    _ -> runParser parseUInt str

parseTuple :: Parser a -> Parser (a, a)
parseTuple ft = Parser $ \str -> case str of
    ('(':xs) -> case runParser ft xs of
        Right (n1, ',':rest1) -> case runParser ft rest1 of
            Right (n2, ')':rest2) -> Right ((n1, n2), rest2)
            Left _ -> Left "none"
        Left _ -> Left "none"
        _ -> Left "none"
    _ -> Left "none"

clearString :: String -> String
clearString [] = []
clearString (':':xs) = clearString xs
clearString ('=':xs) = clearString xs
clearString (' ':xs) = clearString xs
clearString str = str

parseString :: String -> String -> Maybe String
parseString [] str = Just (clearString str)
parseString _ [] = Nothing
parseString (x:xs) (y:ys)
    | x == y = parseString xs ys
    | otherwise = parseString (x:xs) ys

parserString :: String -> Parser String
parserString [] = pure ""
parserString (x:xs) = (parseChar x >> parserString xs) >> pure (x:xs)

clearTab :: [String] -> [String]
clearTab (('\t':ys):xs) = clearTab (ys:xs)
clearTab ((' ':ys):xs) = clearTab (ys:xs)
clearTab (str:xs) = str:clearTab xs
clearTab [] = []
