{-
   Copyright 2014 Jason R Briggs

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

-}

module Proton.XmlInternal where

import Proton.XmlTypes


-- does the char in arg #2 match any of the chars in arg #1?
matches :: String -> Char -> Bool
matches [] _ = False
matches (x:xs) c =
   (x == c) || matches xs c
   

isWhitespace :: Char -> Bool
isWhitespace = matches " \n\t\r"


-- same as span, except the first list is loaded with elements up-to-and-including the match
spanUntil :: (a -> Bool) -> [a] -> ([a], [a])
spanUntil chk [] = ([], [])
spanUntil chk (x:xs) =
    if chk x 
        then ([ x ], xs)
        else do
            let (hd, tl) = spanUntil chk xs
            (x : hd, tl)


-- split a string where the first occurrence of a character (but do not return the char)
splitOn :: Char -> String -> (String, String)
splitOn char s = do
    let (splitA, splitB) = span (/=char) s
    if not (null splitB) 
        then (splitA, tail splitB)
        else (splitA, splitB)


-- get the first char, and then look for the matching closing char (ignoring escaped chars)
splitUntilClose :: String -> (String, String)
splitUntilClose "" = ("", "")
splitUntilClose (c:s) = splitUntilClose' s c ""

-- internal function taking a string to split, a start delimiting character, and the first part of the tuple (used for appending)
-- returns a tuple of two strings
splitUntilClose' :: String -> Char -> String -> (String, String)
splitUntilClose' "" untilc first = (first, "")
splitUntilClose' (c1 : s) untilc first
    | s == "" =
          if c1 == untilc 
            then (first, "") 
            else (first ++ [c1], "")
    | c1 == untilc && (first == "" || last first /= '\\') = 
        (first, s)
    | otherwise =
      do let c2 = head s
         if c2 == untilc && c1 /= '\\' 
             then (first ++ [c1], tail s) 
             else splitUntilClose' s untilc (first ++ [c1])
    
    
-- split used for XML files, to ensure an xml tag element is a distinct member of the list returned
splitText :: String -> [String]
splitText [] = []
splitText (x : xs)
    | x == '<' =
      do let (first, rest) = spanUntil (== '>') xs
         (x : first) : splitText rest
    | isWhitespace x =
      do let (first, rest) = span isWhitespace xs
         (x : first) : splitText rest
    | otherwise =
      do let (first, rest) = span (/= '<') xs
         (x : first) : splitText rest