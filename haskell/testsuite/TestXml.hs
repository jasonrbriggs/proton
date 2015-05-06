module TestXml where

import Test.HUnit

import Text.Proton.Xml
import Text.Proton.XmlTypes

import Utils

basicParseTest = TestCase (do
    let fname = "testsuite/basic.xhtml"
    f <- readFile fname
    x <- parseXmlFile fname
    let s = render x

    checkResult s "testsuite/basic-unprocessed-result.xhtml"
    )

containsAttributeTest1 = TestCase (do
    let notfound1 = containsAttribute "test" []
    let notfound2 = containsAttribute "test" [Attribute "notfound" "test" 1]

    assertEqual "Contains Attr Test1.1 failed" False notfound1
    assertEqual "Contains Attr Test1.2 failed" False notfound2
    )


copyElementsTest = TestCase (do
    let elems1 = copyElements []
    let elems = [Element Raw "test" [] []]
    let elems2 = copyElements elems

    assertEqual "copy elements test1 failed" [] elems1
    assertEqual "copy elements test2 failed" elems elems2
    )


copyElementTest = TestCase (do
    let fname = "testsuite/basic.xhtml"
    f <- readFile fname
    x1 <- parseXmlFile fname
    let s1 = render x1
    let x2 = copyElement x1
    let s2 = render x2
    assertEqual "Copied xml should match original" s1 s2
    )


findAttributeTest1 = TestCase (do
    let notfound1 = findAttribute "test" []
    let notfound2 = findAttribute "test" [Attribute "notfound" "test" 1]

    assertEqual "Find Attr Test1.1 failed" NoAttribute notfound1
    assertEqual "Find Attr Test1.2 failed" NoAttribute notfound2
    )


findAttributeTest2 = TestCase (do
    let atts = [Attribute "test1" "test2" 1, Attribute "test9" "test8" 1]
    let att = findAttribute "test9" atts
    assertEqual "Attribute value should be 'test8'" "test8" (attvalue att)
    )

xmlTests = [TestLabel "Basic Parse Test" basicParseTest,
                      TestLabel "Contains Attribute Test1" containsAttributeTest1,
                      TestLabel "Copy Elements Test" copyElementsTest,
                      TestLabel "Copy Element Test" copyElementTest,
                      TestLabel "Find Attribute Test1" findAttributeTest1,
                      TestLabel "Find Attribute Test2" findAttributeTest2]