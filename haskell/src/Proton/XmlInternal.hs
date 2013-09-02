module Proton.XmlInternal where

import Proton.XmlTypes


-- does the char in arg #2 match any of the chars in arg #1?
matches :: [Char] -> Char -> Bool
matches [] _ = False
matches (x:xs) c = do
   if x == c then True
   else matches xs c
   
   
isWhitespace char = matches [' ','\n', '\t', '\r'] char


-- same as span, except the first list is loaded with elements up-to-and-including the match
spanUntil chk [] = ([], [])
spanUntil chk (x:xs) =
    if chk x 
        then ([ x ], xs)
        else do
            let (hd, tl) = spanUntil chk xs
            ([x] ++ hd, tl)


splitOn :: Char -> String -> (String, String)
splitOn char s = do
    let (splitA, splitB) = span (/=char) s
    if (length splitB) > 0 then (splitA, tail splitB)
    else (splitA, splitB)

    
-- split used for XML files, to ensure an xml tag element is a distinct member of the list returned
splitText :: String -> [String]
splitText [] = []
splitText (x:xs) = 
    if x == '<' then do
        let (first, rest) = spanUntil (=='>') xs
        [x : first] ++ splitText rest
    else do
        if isWhitespace x then do
            let (first, rest) = span (isWhitespace) xs
            [x : first] ++ splitText rest
        else do
            let (first, rest) = span (/='<') xs
            [x : first] ++ splitText rest