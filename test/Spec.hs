{--
-- MYPANDOC PROJECT, 2024
-- units test
-- File description:
-- mypandoc test proj
--}

import Test.HUnit

import ParseJSONtoMD (cleanString,
    isNeedClear,
    getTitle,
    getAuthor,
    getDate,
    parseJsonHeaderToMDHeader,
    parseCodeBlock,
    takeOffCodeBlock,
    getFirstIndex,
    getLink,
    goToNextScope,
    parseList,
    appendToLast)

unitsTests :: Test

unitsTests = TestList [
    testCleanStr,
    testisNeedClear,
    testgetTitle,
    testgetDate,
    testgetAuthor,
    testJsonHeader,
    testParseCodeBlock,
    testtakeOffCodeBlock,
    testgetFirstIndex,
    testgetLink,
    testgetLinkIMG,
    testgoToNextScope,
    testparseList,
    testappendToLast
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
    let res = parseJsonHeaderToMDHeader ["\"title\": \"Syntaxe JSON\",", "\"author\": \"Edward Lalande\",","\"date\": \"17-10-2023\""]
    assertEqual "parseJsonHeaderToMDHeader is not good" ["title: Syntaxe JSON", "author: Edward Lalande", "date: 17-10-2023", "---"] res

testParseCodeBlock :: Test
testParseCodeBlock = TestCase $ do
    let res = parseCodeBlock ["[", "\"test\"","],", "[", "\"test2\"","]"] []
    assertEqual "parseCodeBlock failed" ["```", "test", "test2", "```", ""] res

testtakeOffCodeBlock :: Test
testtakeOffCodeBlock = TestCase $ do
    let res = takeOffCodeBlock ["[", "]", "[", "]", "[", "],", "a oui oui"]
    assertEqual "takeoffCodeBlock failed" ["a oui oui"] res

testgetFirstIndex :: Test
testgetFirstIndex = TestCase $ do
    let res = getFirstIndex ["le monde est méchant", "c terrible"]
    assertEqual "getFirstIndex failed" "le monde est méchant" res

testgetLink :: Test
testgetLink = TestCase $ do
    let res = getLink ["\"url\": \"test.com\",", "\"content\": [", "\"oui\""] "" "" False
    assertEqual "getLink failed" "[oui](test.com)" res

testgetLinkIMG :: Test
testgetLinkIMG = TestCase $ do
    let res = getLink ["\"url\": \"test.com\",", "\"alt\": [", "\"oui\""] "" "" True
    assertEqual "getLink failed" "![oui](test.com)" res

testgoToNextScope :: Test
testgoToNextScope = TestCase $ do
    let res = goToNextScope ["debut", "fun", "}", "ouais ouais", "le monde est méchant"]
    assertEqual "gotToNextScope failed" ["ouais ouais", "le monde est méchant"] res

testparseList :: Test
testparseList = TestCase $ do
    let res = parseList ["[", "\"item 1\"", "],", "[", "\"item 2\"", "]"]
    assertEqual "parseList failed" ["- item 1\"", "- item 2\"", ""] res

testappendToLast :: Test
testappendToLast = TestCase $ do
    let res = appendToLast ["pandoc", "|", " est", "|", " si", "|", " mal", "|" ," foutu"]
    assertEqual "appendToList failed" ["pandoc est si mal foutu"] res

main :: IO ()
main = do
    _ <- runTestTT unitsTests
    return ()
