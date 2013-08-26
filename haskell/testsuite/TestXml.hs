module TestXml where

import Test.HUnit

import Proton.Xml

basicParseTest = TestCase (do
    let fname = "testsuite/basic.xhtml"
    f <- readFile fname
    x <- parseXmlFile fname
    let s = render x

    unprocessed <- readFile "testsuite/basic-unprocessed-result.xhtml"

    assertEqual "Rendered xml should be the same as source" unprocessed s)


findAttributeTest = TestCase (do
    let atts = [(Attribute "test1" "test2" 1), (Attribute "test9" "test8" 1)]
    let att = findAttribute "test9" atts
    assertEqual "Attribute value should be 'test8'" "test8" (attvalue att)
    )


copyElementTest = TestCase (do
    let fname = "testsuite/basic.xhtml"
    f <- readFile fname
    x1 <- parseXmlFile fname
    let s1 = render x1
    let x2 = copyElement x1 1
    let s2 = render x2
    assertEqual "Copied xml should match original" s1 s2
    )


xml_tests = TestList [TestLabel "Basic Parse Test" basicParseTest,
                    TestLabel "Find Attribute Test" findAttributeTest,
                    TestLabel "Copy Element Test" copyElementTest]