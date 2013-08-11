module Proton.Xml (
    Element(..),
    Attribute,
    parseXml,
    parseXmlFile,
    render
) where

import Data.String.Utils
import Data.List (intercalate)
import Text.Regex

data Attribute = Attribute String String
                 deriving (Show)

data Element = Root [Element]
             | RawElement String
             | Element String [Attribute] [Element]
             | ClosedElement String [Attribute]
             | TextElement String
               deriving (Show)


-- does the char in arg #2 match any of the chars in arg #1?
matches :: [Char] -> Char -> Bool
matches [] c = False
matches (x:xs) c = do
   if x == c then True
   else matches xs c


isWhitespace char = matches [' ','\n', '\t', '\r'] char


-- same as span, except the first list is loaded with elements up-to-and-including the match
spanUntil chk [] = ([], [])
spanUntil chk (x:xs) =
   if chk x then ([ x ], xs)
   else do
       let (hd, tl) = spanUntil chk xs
       ([x] ++ hd, tl)


splitOn         :: Char -> String -> (String, String)
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


-- todo: fix escaped double quote in attr value
parseAttributes      :: String -> [Attribute]
parseAttributes ""   = []
parseAttributes ">"  = []
parseAttributes " />" = []
parseAttributes "/>" = []
parseAttributes s    = do
    let news = dropWhile (matches [' ', '"']) s
    let (name, maybeValue) = splitOn '=' news
    let (value, rest) = span (not . matches ['"']) $ dropWhile (matches ['=', '"', '>']) maybeValue
    [Attribute name value] ++ if rest /= "" then parseAttributes (tail rest) else []


-- return the tag name, and then the remaining content of the element
parseTag :: String -> (String, String)
parseTag s = do
    let (_, remainder) = span (matches ['<','/']) s
    span (not . matches [' ','>', '/']) remainder


-- internal xml parser code
parse :: [String] -> ([Element], [String])
parse [] = ([], [])
parse (x:xs) = do
    let first = head x
    let sec = head (tail x)
    let seclst = last (init x)
    let lst = last x
    
    case (first, sec, seclst, lst) of
        ('<', '?', _, _)   -> do
            let (parsed, remaining) = parse xs
            ([RawElement x] ++ parsed, remaining)
        ('<', '!', _, _)   -> do
            let (parsed, remaining) = parse xs
            ([RawElement x] ++ parsed, remaining)
        ('<', _, '/', '>') -> do
            let (tag, tagcontent) = parseTag x
            let attributes = parseAttributes tagcontent
            let (parsed, remaining) = parse xs
            ([ClosedElement tag attributes] ++ parsed, remaining)
        ('<', '/', _, '>') -> ([], xs)
        ('<', _, _, '>')   -> do
            let (tag, tagcontent) = parseTag x
            let attributes = parseAttributes tagcontent
            let (children, siblings) = parse xs
            let (parsed, remaining) = parse siblings
            ([Element tag attributes children] ++ parsed, remaining)
        (_, _, _, _)       -> do
            let (parsed, remaining) = parse xs
            ([TextElement x] ++ parsed, remaining)


parseXml :: String -> Element
parseXml xml = do
   let sp = splitText xml
   let (parsed, _) = parse sp
   Root (parsed)


parseXmlFile :: String -> IO Element
parseXmlFile fname = do    
   file <- readFile fname
   let e = parseXml file
   return e


renderNoop :: String -> [Attribute] -> [Element] -> (String, [Attribute], [Element])
renderNoop s as xs = (s, as, xs)


render :: Element -> String
render (RawElement s) = s
render (ClosedElement s atts) = renderClosed s atts renderNoop
render (TextElement s) = s
render (Root xs) = renderList xs
render (Element s as xs) = "<" ++ s ++ (renderAttributeList as) ++ ">" ++ (renderList xs) ++ "</" ++ s ++ ">"


render' :: Element -> (String -> [Attribute] -> [Element] -> (String, [Attribute], [Element])) -> String
render' (RawElement s) fn = s
render' (ClosedElement s atts) fn = renderClosed s atts fn
render' (TextElement s) fn = s
render' (Root xs) fn = renderList xs
render' (Element s as xs) fn = "<" ++ s ++ (renderAttributeList as) ++ ">" ++ (renderList xs) ++ "</" ++ s ++ ">"


renderClosed :: String -> [Attribute] -> (String -> [Attribute] -> [Element] -> (String, [Attribute], [Element])) -> String
renderClosed tag atts fn = do
    let none = [(TextElement "")]
    let (newtag, newatts, _) = fn tag atts none
    "<" ++ newtag ++ (renderAttributeList newatts) ++ " />"


renderOpen :: String -> [Attribute] -> [Element] -> (String -> [Attribute] -> [Element] -> (String, [Attribute], [Element])) -> String
renderOpen tag atts xs fn = do
    let (newtag, newatts, newxs) = fn tag atts xs
    "<" ++ newtag ++ (renderAttributeList newatts) ++ ">" ++ (renderList newxs) ++ "</" ++ newtag ++ ">"


renderAttribute :: Attribute -> String
renderAttribute (Attribute name val) = " " ++ name ++ "=\"" ++ val ++ "\""


renderList :: [Element] -> String
renderList [] = ""
renderList (x:xs) = render x ++ (intercalate "" (map render xs))


renderAttributeList :: [Attribute] -> String
renderAttributeList [] = ""
renderAttributeList (x:xs) = renderAttribute x ++ (intercalate "" (map renderAttribute xs))
