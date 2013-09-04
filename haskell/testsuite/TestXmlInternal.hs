module TestXmlInternal where

import Test.HUnit

import Proton.XmlTypes
import Proton.XmlInternal
import Proton.Xml


matchesTest = TestCase (do
    let test1 = matches "" 'a'
    let test2 = matches "abc" 'a'
    let test3 = matches "abc" 'c'
    let test4 = matches "abc" 'z'
    
    assertEqual "Test 1 should not match" False test1
    assertEqual "Test 2 should match" True test2
    assertEqual "Test 3 should match" True test3
    assertEqual "Test 4 should not match" False test4
    )


isWhitespaceTest = TestCase (do
    assertEqual "Space should be true" True $ isWhitespace ' '
    assertEqual "Tab should be true" True $ isWhitespace '\t'
    assertEqual "Newline should be true" True $ isWhitespace '\n'
    assertEqual "Carriage should be true" True $ isWhitespace '\r'
    assertEqual "Non-whitespace char should be false" False $ isWhitespace 'A'
    )


spanUntilTest1 = TestCase (do
    let (a,b) = spanUntil (==' ') ""
    assertEqual "Empty string should result in empty span" ([], []) (a,b)
    )

    
spanUntilTest2 = TestCase (do
    let (a,b) = spanUntil (==' ') "abc def"
    assertEqual "Span test2 failed" ("abc ", "def") (a,b)
    )

    
splitOnTest1 = TestCase (do
    let (a,b) = splitOn ' ' ""
    assertEqual "Spliton Test1 failed" ("", "") (a, b)
    )


splitOnTest2 = TestCase (do
    let (a,b) = splitOn ' ' "abcd efgh"
    assertEqual "Spliton Test1 failed" ("abcd", "efgh") (a, b)
    )


splitTextTest = TestCase (do
    let test1 = splitText "<a>test</a><b>test</b>"
    let test2 = splitText "<a href=\"blah\">test</a>"
    
    assertEqual "Split Text failed" ["<a>","test","</a>","<b>","test","</b>"] test1
    assertEqual "Split Text failed" ["<a href=\"blah\">","test","</a>"] test2
    )


splitUntilCloseTest = TestCase (do
    let (splitA1, splitB1) = splitUntilClose ""
    let (splitA2, splitB2) = splitUntilClose "\"abc def\""
    let (splitA3, splitB3) = splitUntilClose "\"abc def\" id=\"blah\""
    let (splitA4, splitB4) = splitUntilClose "\"abc\\\"def\\\"\" id=\"blah\""
    let (splitA5, splitB5) = splitUntilClose "'abc\\'def\\'' id=\"blah\""
    let (splitA6, splitB6) = splitUntilClose "\"\">"
    
    assertEqual "Split 1 failed" (splitA1, splitB1) ("", "")
    assertEqual "Split 2 failed" (splitA2, splitB2) ("abc def", "")
    assertEqual "Split 3 failed" (splitA3, splitB3) ("abc def", " id=\"blah\"")
    assertEqual "Split 4 failed" (splitA4, splitB4) ("abc\\\"def\\\"", " id=\"blah\"")
    assertEqual "Split 5 failed" (splitA5, splitB5) ("abc\\'def\\'", " id=\"blah\"")
    assertEqual "Split 6 failed" (splitA6, splitB6) ("", ">")
    )


xml_tests = TestList [TestLabel "Matches Test" matchesTest,
                      TestLabel "Whitespace Test" isWhitespaceTest,
                      TestLabel "Span Test 1" spanUntilTest1,
                      TestLabel "Span Test 2" spanUntilTest2,
                      TestLabel "Split On Test 1" splitOnTest1,
                      TestLabel "Split On Test 2" splitOnTest2,
                      TestLabel "Split Text Test" splitTextTest,
                      TestLabel "Split Until Close Test" splitUntilCloseTest]