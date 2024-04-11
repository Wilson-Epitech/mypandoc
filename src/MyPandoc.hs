{--
-- MYPANDOC PROJECT, 2024
-- myPandoc
-- File description:
-- pandoc proj
--}

module MyPandoc (parseChar, parseAnyChar, parseOr, myPandoc) where

import System.Environment
import ErrorHandling

-- beginning lib parsing --

type Parser a = String -> Maybe (a, String)

parseChar :: Char -> Parser Char
parseChar c (x:xs)
    | x == c = Just (c, xs)
    | otherwise = Nothing
parseChar _ _ = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar rest (x:xs)
    | x `elem` rest = Just (x, xs)
    | otherwise = Nothing
parseAnyChar _ _ = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr parse1 parse2 char = case parse1 char of
    Just (res, rest) -> Just (res, rest)
    Nothing -> parse2 char

-- end lib parsing --

myPandoc' :: [String] -> IO ()
myPandoc' arg = putStrLn "good args" -- full algo ici

myPandoc :: IO ()
myPandoc = do
    args <- getArgs
    if length args /= 4 && length args /= 6
        then checkErrors args
    else myPandoc' args