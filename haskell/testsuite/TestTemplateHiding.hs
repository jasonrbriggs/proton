module TestTemplateHiding where

import Test.HUnit

import Proton.Template
import Utils


hidingTest = TestCase (do
    tmps <- loadTemplates "testsuite"
    tmp <- getTemplate tmps "testsuite/hiding.xhtml"
    
    tmp <- setElementValue tmp "title" "Hiding Xhtml Page" 0
    tmp <- hideElement tmp "hidden-element" 0

    s <- renderTemplate tmp
    
    checkFile <- readFile "testsuite/hiding-result.xhtml"
    let checkInput = stripWhitespace s
    let checkOutput = stripWhitespace checkFile
    assertEqual "Output does not match" checkOutput checkInput
    )

    
template_tests = TestList [
        TestLabel "Hiding Test 1" hidingTest
        ]
