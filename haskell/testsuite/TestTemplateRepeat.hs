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
    tmp <- repeatElement tmp "list-item" 0 5    
    tmp <- repeatApplyValues tmp [1..5]

    s <- renderTemplate tmp
    
    checkResult s "testsuite/repeat-result.xhtml"
    )

repeatTest2 = TestCase (do
    tmps <- loadTemplates "testsuite"
    tmp <- getTemplate tmps "testsuite/repeat.xhtml"
    
    tmp <- setElementValue tmp "title" "Repeating Xhtml Page" 0
    tmp <- setElementValue tmp "link" "This is a link to Google" 0
    tmp <- setAttributeValue tmp "link" "href" "http://www.google.com" 0
    
    tmp <- setElementValues tmp "list-item" (map (\x -> "test" ++ (show x)) [0..4])

    s <- renderTemplate tmp
    
    checkResult s "testsuite/repeat-result.xhtml"
    )
    
repeatTestComplex = TestCase (do
    {-
    

    # fourth post
    tmp.set_value('content', 'lorum ipsom 4', 3)
    tmp.repeat('taglinks', 3, 4)
    tmp.set_value('taglink', 'testtag5', 4)
    tmp.set_attribute('taglink', 'href', '/tags/testtag5', 4)
    tmp.set_value('taglink', 'testtag6', 5)
    tmp.set_attribute('taglink', 'href', '/tags/testtag6', 5)
    tmp.set_value('taglink', 'testtag7', 6)
    tmp.set_attribute('taglink', 'href', '/tags/testtag7', 6)

    # fifth post
    tmp.set_value('content', 'lorum ipsom 5', 4)
    tmp.repeat('taglinks', 2, 7)
    tmp.set_value('taglink', 'testtag8', 7)
    tmp.set_attribute('taglink', 'href', '/tags/testtag8', 7)
    tmp.set_value('taglink', 'testtag9', 8)
    tmp.set_attribute('taglink', 'href', '/tags/testtag9', 8)
    -}
    tmps <- loadTemplates "testsuite"
    tmp <- getTemplate tmps "testsuite/repeat-complex.xhtml"
    
    tmp <- repeatElement tmp "posts" 0 5
    
    tmp <- setElementValue tmp "content" "lorum ipsom 1" 1
    tmp <- repeatElement tmp "taglinks" 1 1
    tmp <- setElementValue tmp "taglink" "testtag1" 1
    tmp <- setAttributeValue tmp "taglink" "href" "/tags/testtag1" 1
    
    tmp <- setElementValue tmp "content" "lorum ipsom 2" 2
    tmp <- repeatElement tmp "taglinks" 2 1
    tmp <- setElementValue tmp "taglink" "testtag2" 2
    tmp <- setAttributeValue tmp "taglink" "href" "/tags/testtag2" 2
    
    tmp <- setElementValue tmp "content" "lorum ipsom 3" 3
    tmp <- repeatElement tmp "taglinks" 3 2
    tmp <- setElementValue tmp "taglink" "testtag3" 3
    tmp <- setAttributeValue tmp "taglink" "href" "/tags/testtag3" 3
    tmp <- setElementValue tmp "taglink" "testtag4" 4
    tmp <- setAttributeValue tmp "taglink" "href" "/tags/testtag4" 4

    
    s <- renderTemplate tmp
    
    putStrLn s
    
    assertEqual "" "" ""
    )
    
template_tests = TestList [
        TestLabel "Repeat Test" repeatTest,
        TestLabel "Repeat Test 2" repeatTest2
        --TestLabel "Repeat Test Complex" repeatTestComplex
        ]
