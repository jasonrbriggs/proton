{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types (status200, status404)
import Network.Wai.Handler.Warp (run)

import Data.ByteString.Lazy.Char8

import Text.Proton.Template


consoleLog :: String -> IO ()
consoleLog msg = Prelude.putStrLn msg


application :: Templates -> Application
application tmps request respond = do
    case rawPathInfo request of
        "/"     -> do
            resp <- handleGetBasic tmps
            respond $ resp
        _       -> respond $ notFound


handleGetBasic :: Templates -> IO Response
handleGetBasic tmps = do
    tmp <- getTemplate tmps "basic.xhtml"
    tmp <- setElementValue tmp "title" "Basic Xhtml Page" 0
    tmp <- setElementValue tmp "content" "Content goes here" 0
    tmp <- setElementValue tmp "link" "Link goes here" 0
    s <- renderTemplate tmp
    return (responseLBS status200 [("Content-Type", "text/html")] $ pack s)


notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

    
main = do
    tmps <- loadTemplates "."
    consoleLog "Started BasicWebExample"
    run 3000 (application tmps)