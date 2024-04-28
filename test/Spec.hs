{--
-- MYPANDOC PROJECT, 2024
-- units test
-- File description:
-- mypandoc test proj
--}

import Test.HUnit (Test (TestList, TestCase), assertEqual, runTestTT)
import MyPandoc
import ParseJSONtoMD (cleanString, isNeedClear, getTitle, getAuthor, getDate,
    parseJsonHeaderToMDHeader)

unitsTests :: Test

unitsTests = TestList [
    testCleanStr,
    testisNeedClear,
    testgetTitle,
    testgetDate,
    testgetAuthor
    ]

testCleanStr :: Test
testCleanStr = TestCase $ do
    let res = cleanString "\"hello\""
    let resNormal = cleanString "hello"
    assertEqual "cleanString should be (hello)" "hello" res
    assertEqual "cleanString should be (hello)" "hello" resNormal

testisNeedClear :: Test
testisNeedClear = TestCase $ do
    let res = isNeedClear "hello"
    let resNormal = isNeedClear "hello\""
    assertEqual "isNeedClear should be False" False res
    assertEqual "isNeedClear should be True" True resNormal

testgetTitle :: Test
testgetTitle = TestCase $ do
    let res = getTitle "\"el titelo\","
    assertEqual "getTitle should be title: el titelo" "title: el titelo" res

testgetAuthor :: Test
testgetAuthor = TestCase $ do
    let res = getAuthor "\"el Author\","
    assertEqual "getAuthor should be author: el Author" "author: el Author" res

testgetDate :: Test
testgetDate = TestCase $ do
    let res = getDate "\"17-10-2003\""
    assertEqual "getDate should be date: 17-10-2003" "date: 17-10-2003" res

testJsonHeader :: Test
testJsonHeader = TestCase $ do
    let res = parseJsonHeaderToMDHeader ["\"title\": \"Syntaxe JSON\"", "\"author\": \"Edward Lalande\"","\"date\": \"17-10-2023\""]
    assertEqual "parseJsonHeaderToMDHeader is not good" ["---", "title: Syntaxe JSON", "author: Edward Lalande", "date: 17-10-2023"] res

testparseCodeBlock :: Test
testparseCodeBlock = TestCase $ do
    let res = parseCodeBlock

main :: IO ()
main = do
    _ <- runTestTT unitsTests
    return ()
