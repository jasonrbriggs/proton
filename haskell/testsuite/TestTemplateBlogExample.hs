module TestTemplateBlogExample where

import Test.HUnit

import Text.Proton.Template
import Utils

data TextLink = TextLink String String

setTextAndLink :: Template -> String -> [TextLink] -> IO Template
setTextAndLink tmp name items = do
    tmp <- repeatElement tmp name 0 (toInteger $ length items)
    setTextAndLink' tmp name items 1

setTextAndLink' :: Template -> String -> [TextLink] -> Integer -> IO Template
setTextAndLink' tmp _ [] _ = return tmp
setTextAndLink' tmp name (x:xs) num = do
    let (TextLink text link) = x
    tmp <- setElementValue tmp (name ++ ":text") text num
    tmp <- setAttributeValue tmp name "href" link num
    tmp <- setTextAndLink' tmp name xs (num + 1)
    return tmp


blogTest = TestCase (do
    tmps <- loadTemplates "testsuite"
    tmp <- getTemplate tmps "blogexample.xhtml"
    
    tmp <- setElementValue tmp "title" "My Test Blog" 0
    
    tmp <- setTextAndLink tmp "menu" [ TextLink "Home" "/home", TextLink "About" "/about", TextLink "Admin" "/admin"]
    tmp <- setTextAndLink tmp "blogroll" [ TextLink "Some blog" "http://www.someblog.com", TextLink "Another blog" "http://www.anotherblog.com" ]
    
    tmp <- setElementValue tmp "post:title" "My First Post" 0
    tmp <- setElementValue tmp "post:date" "10-Jan-2009 21:47pm" 0
    tmp <- setElementValue tmp "post:author" "joe@bloggs.com" 0
    tmp <- setElementValue tmp "post:content" "This is my first post...</p><p>...and what a great post it is." 0

    s <- renderTemplate tmp
    
    checkResult s "testsuite/blogexample-result.xhtml"
    )

    
templateTests = [ TestLabel "Blog Example Test" blogTest ]
