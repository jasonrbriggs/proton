module TestXml where

import Test.HUnit

import Proton.Xml

basicParseCheck = TestCase (do
        let fname = "testsuite/basic.xhtml"
        f <- readFile fname
        x <- parseXmlFile fname
        let s = render x
        assertEqual "Rendered xml should be the same as source" f s)
    
tests = TestList [TestLabel "Basic Parse Check" basicParseCheck]