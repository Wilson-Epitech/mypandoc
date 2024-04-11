{--
-- MYPANDOC PROJECT, 2024
-- units test
-- File description:
-- mypandoc test proj
--}

import Test.HUnit (Test (TestList, TestCase), assertEqual, runTestTT)
import MyPandoc

unitsTests :: Test

unitsTests = TestList [
    unitsTestsParseChar,
    unitsTestsParseCharNothing,
    unitsTestsParseAnyChar,
    unitsTestsParseAnyCharNothing,
    unitsTestsParseOrFirst,
    unitsTestsParseOrSecond,
    unitsTestsParseOrFail
    ]

unitsTestsParseChar :: Test
unitsTestsParseChar = TestCase $ do
    let result = parseChar 'a' "abcd"
    assertEqual "parseChar result should be Just ('a', \"bcd\")" (Just ('a', "bcd")) result

unitsTestsParseCharNothing :: Test
unitsTestsParseCharNothing = TestCase $ do
    let result = parseChar 'e' "abcd"
    assertEqual "parseChar result should be Nothing" Nothing result

unitsTestsParseAnyCharNothing :: Test
unitsTestsParseAnyChar = TestCase $ do
    let result = parseAnyChar "abc" "def"
    assertEqual "parseAnyChar result should be Nothing" Nothing result

unitsTestsParseAnyChar :: Test
unitsTestsParseAnyCharNothing = TestCase $ do
    let result = parseAnyChar "abc" "bdef"
    assertEqual "parseAnyChar result should be Just ('b', \"def\")" (Just ('b', "def")) result

unitsTestsParseOrFirst :: Test
unitsTestsParseOrFirst = TestCase $ do
    let result = parseOr (parseChar 'a') (parseChar 'b') "abcd"
    assertEqual "parseOr result should be Just ('a', \"bcd\")" (Just ('a', "bcd")) result

unitsTestsParseOrSecond :: Test
unitsTestsParseOrSecond = TestCase $ do
    let result = parseOr (parseChar 'a') (parseChar 'b') "bcda"
    assertEqual "parseOr result should be Just ('b', \"cda\")" (Just ('b', "cda")) result

unitsTestsParseOrFail :: Test
unitsTestsParseOrFail = TestCase $ do
    let result = parseOr (parseChar 'a') (parseChar 'b') "xyz"
    assertEqual "parseOr result should be Nothing" Nothing result

main :: IO ()
main = do
    _ <- runTestTT unitsTests
    return ()
