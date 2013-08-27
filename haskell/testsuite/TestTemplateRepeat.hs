module TestTemplateRepeat where

import Test.HUnit

import Proton.Template
import Utils


repeatApplyValues tmp [] = return tmp
repeatApplyValues tmp (x:xs) = do
    tmp <- setElementValue tmp "list-item" ("test" ++ (show $ x - 1)) x 
    repeatApplyValues tmp xs


repeatTest = TestCase (do
    tmps <- loadTemplates "testsuite"
    tmp <- getTemplate tmps "testsuite/repeat.xhtml"
    
    tmp <- setElementValue tmp "title" "Repeating Xhtml Page" 0
    tmp <- setElementValue tmp "link" "This is a link to Google" 0
    tmp <- setAttributeValue tmp "link" "href" "http://www.google.com" 0
    tmp <- repeatElement tmp "list-item" 5    
    tmp <- repeatApplyValues tmp [1..5]

    s <- renderTemplate tmp
    
    checkFile <- readFile "testsuite/repeat-result.xhtml"
    let checkInput = stripWhitespace s
    let checkOutput = stripWhitespace checkFile
    assertEqual "Output does not match" checkOutput checkInput
    )

repeatTest2 = TestCase (do
    tmps <- loadTemplates "testsuite"
    tmp <- getTemplate tmps "testsuite/repeat.xhtml"
    
    tmp <- setElementValue tmp "title" "Repeating Xhtml Page" 0
    tmp <- setElementValue tmp "link" "This is a link to Google" 0
    tmp <- setAttributeValue tmp "link" "href" "http://www.google.com" 0
    
    tmp <- setElementValues tmp "list-item" (map (\x -> "test" ++ (show x)) [0..4])

    s <- renderTemplate tmp
    
    checkFile <- readFile "testsuite/repeat-result.xhtml"
    let checkInput = stripWhitespace s
    let checkOutput = stripWhitespace checkFile
    assertEqual "Output does not match" checkOutput checkInput
    )
    
template_tests = TestList [
        TestLabel "Repeat Test" repeatTest,
        TestLabel "Repeat Test 2" repeatTest2
        ]
