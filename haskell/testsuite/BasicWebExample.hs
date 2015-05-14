{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types (status200, status404)
import Network.Wai.Handler.Warp (run)

import Data.ByteString.Lazy.Char8

import Text.Proton.Template

application :: Application
application request respond = do
    tmps <- loadTemplates "."
    tmp <- getTemplate tmps "basic.xhtml"
    tmp <- setElementValue tmp "title" "Basic Xhtml Page" 0
    tmp <- setElementValue tmp "content" "Content goes here" 0
    tmp <- setElementValue tmp "link" "Link goes here" 0
    s <- renderTemplate tmp
    respond $ responseLBS status200 [("Content-Type", "text/html")] $ pack s
    
main = run 3000 application