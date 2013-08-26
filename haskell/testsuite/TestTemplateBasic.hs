module TestTemplateBasic where

import Test.HUnit

import Proton.Template
import Utils


basicTest = TestCase (do
    tmps <- loadTemplates "testsuite"
    tmp <- getTemplate tmps "testsuite/basic.xhtml"
    tmp <- setElementValue tmp "title" "Basic Xhtml Page" 0
    tmp <- setElementValue tmp "content" "Content goes here" 0
    tmp <- setElementValue tmp "link" "Link goes here" 0
    tmp <- setAttributeValue tmp "link" "href" "http://www.google.com" 0
    s <- renderTemplate tmp
    
    checkFile <- readFile "testsuite/basic-result.xhtml"
    let checkInput = stripWhitespace s
    let checkOutput = stripWhitespace checkFile
    assertEqual "Output does not match" checkOutput checkInput
    )


basicTest2 = TestCase (do
    tmps <- loadTemplates "testsuite"
    tmp <- getTemplate tmps "testsuite/basic.xhtml"
    tmp <- setElementValue tmp "title" "Basic Xhtml Page1" 1
    tmp <- setElementValue tmp "title" "Basic Xhtml Page2" 2
    tmp <- setElementValue tmp "content" "Content goes here" 0
    tmp <- setElementValue tmp "link" "Link goes here" 0
    tmp <- setAttributeValue tmp "link" "href" "http://www.google.com" 0
    s <- renderTemplate tmp
    
    checkFile <- readFile "testsuite/basic-result2.xhtml"
    let checkInput = stripWhitespace s
    let checkOutput = stripWhitespace checkFile
    assertEqual "Output does not match" checkOutput checkInput
    )


basicWithNamespaceTest = TestCase (do
    tmps <- loadTemplates "testsuite"
    tmp <- getTemplate tmps "testsuite/basic-with-namespace.xml"
    tmp <- repeatElement tmp "urls" 2
    tmp <- setElementValue tmp "url" "http://testhost/test1.html" 1
    tmp <- setElementValue tmp "last-modified" "2012-01-01T23:59:59" 1
    tmp <- setElementValue tmp "url" "http://testhost/test2.html" 2
    tmp <- setElementValue tmp "last-modified" "2012-01-02T12:59:59" 2
    s <- renderTemplate tmp
    
    checkFile <- readFile "testsuite/basic-with-namespace-result.xml"
    let checkInput = stripWhitespace s
    let checkOutput = stripWhitespace checkFile
    assertEqual "Output does not match" checkOutput checkInput
    )


template_tests = TestList [
        TestLabel "Basic Template Test" basicTest,
        TestLabel "Basic Template Test2" basicTest2,
        TestLabel "Basic Namespace Test" basicWithNamespaceTest
        ]
