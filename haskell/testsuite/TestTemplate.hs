module TestTemplate where

import Test.HUnit

import Proton.Template

basicTest = TestCase (do
    tmps <- templates
    tmps2 <- loadTemplate tmps "testsuite/basic.xhtml"
    tmp <- getTemplate tmps2 "testsuite/basic.xhtml"
    tmp <- setElementValue tmp "title" "Basic Xhtml Page1" 1
    tmp <- setElementValue tmp "title" "Basic Xhtml Page2" 2
    tmp <- setElementValue tmp "content" "Content goes here" 0
    tmp <- setElementValue tmp "link" "Link goes here" 0
    s <- renderTemplate tmp
    putStrLn s
    assertEqual "" "" "")
    
template_tests = TestList [TestLabel "Basic Template Test" basicTest]