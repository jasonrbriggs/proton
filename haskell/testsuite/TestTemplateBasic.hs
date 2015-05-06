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
    tmp <- setAttributeValue tmp "link" "href" "http://www.duckduckgo.com" 0
    s <- renderTemplate tmp
    
    checkResult s "testsuite/basic-result.xhtml"
    )


basicTest2 = TestCase (do
    tmps <- loadTemplates "testsuite"
    tmp <- getTemplate tmps "testsuite/basic.xhtml"
    tmp <- setElementValue tmp "title" "Basic Xhtml Page1" 1
    tmp <- setElementValue tmp "title" "Basic Xhtml Page2" 2
    tmp <- setElementValue tmp "content" "Content goes here" 0
    tmp <- setElementValue tmp "link" "Link goes here" 0
    tmp <- setAttributeValue tmp "link" "href" "http://www.duckduckgo.com" 0
    tmp <- setAttributeValue tmp "link" "class" "testclass" 0
    s <- renderTemplate tmp
    
    checkResult s  "testsuite/basic-result2.xhtml"
    )


basicWithNamespaceTest = TestCase (do
    tmps <- loadTemplates "testsuite"
    tmp <- getTemplate tmps "testsuite/basic-with-namespace.xml"
    tmp <- repeatElement tmp "urls" 0 2
    tmp <- setElementValue tmp "url" "http://testhost/test1.html" 1
    tmp <- setElementValue tmp "last-modified" "2012-01-01T23:59:59" 1
    tmp <- setElementValue tmp "url" "http://testhost/test2.html" 2
    tmp <- setElementValue tmp "last-modified" "2012-01-02T12:59:59" 2
    s <- renderTemplate tmp
    
    checkResult s "testsuite/basic-with-namespace-result.xml"
    )

includeTest = TestCase (do
    tmps <- loadTemplates "testsuite"
    tmp <- getTemplate tmps "testsuite/include1.xhtml"
    tmp <- setElementValue tmp "title" "Page Title" 0
    tmp <- include tmp "include-content" "testsuite/include2.xhtml" 0
    tmp <- setElementValue tmp "para1" "First paragraph of text" 0
    tmp <- setElementValue tmp "para2" "Second paragraph of text" 0
    
    s <- renderTemplate tmp
    
    checkResult s "testsuite/include-result.xhtml"
    )


applyTwoTemplates tmp = do
    tmp <- repeatElement tmp "list" 0 2
    tmp <- setElementValue tmp "listid" "1" 1
    tmp <- setElementValue tmp "listid" "2" 2
    tmp <- setElementValue tmp "listval" "my item A" 1
    tmp <- setElementValue tmp "listval" "my item B" 2
    tmp <- setAttributeValue tmp "listid" "id" "1" 1
    tmp <- setAttributeValue tmp "listid" "id" "2" 2
    return tmp


twoTemplatesTest = TestCase (do
    tmps <- loadTemplates "testsuite"
    tmp1 <- getTemplate tmps "testsuite/twotemplates.xml"
    tmp1 <- applyTwoTemplates tmp1
    tmp2 <- getTemplate tmps "testsuite/twotemplates.xhtml"
    tmp2 <- applyTwoTemplates tmp2
    
    s1 <- renderTemplate tmp1
    s2 <- renderTemplate tmp2
    
    checkResult s1 "testsuite/twotemplates-result.xml"
    checkResult s2 "testsuite/twotemplates-result.xhtml"
    )

templateTests = [
        TestLabel "Basic Template Test" basicTest,
        TestLabel "Basic Template Test2" basicTest2,
        TestLabel "Basic Namespace Test" basicWithNamespaceTest,
        TestLabel "Include Test" includeTest,
        TestLabel "Two Templates Test" twoTemplatesTest
        ]
