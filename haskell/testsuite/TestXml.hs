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
    let atts = [(Attribute "test1" "test2" 1), (Attribute "test9" "test8" 1)]
    let att = findAttribute "test9" atts
    case att of
        Just a -> assertEqual "Attribute value should be 'test8'" "test8" (attvalue a)
        Nothing -> assertFailure "Expecting a value"
    )

xml_tests = TestList [TestLabel "Basic Parse Test" basicParseTest,
                TestLabel "Find Attribute Test" findAttributeTest]