module TestXml where

import Test.HUnit

import Proton.Xml

basicParseTest = TestCase (do
    let fname = "testsuite/basic.xhtml"
    f <- readFile fname
    x <- parseXmlFile fname
    let s = render x
    putStrLn s
    assertEqual "Rendered xml should be the same as source" f s)

findAttributeTest = TestCase (do
    let atts = [(Attribute "test1" "test2"), (Attribute "test9" "test8")]
    let val = findAttributeValue "test9" atts
    case val of
        Just v -> assertEqual "Attribute value should be 'test9'" "test8" v
        Nothing -> assertFailure "Expecting a value"
    )

xml_tests = TestList [TestLabel "Basic Parse Test" basicParseTest,
                TestLabel "Find Attribute Test" findAttributeTest]