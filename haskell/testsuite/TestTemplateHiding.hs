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
    
    checkResult s "testsuite/hiding-result.xhtml"
    )

    
templateTests = [
        TestLabel "Hiding Test 1" hidingTest
        ]
