module Proton.Xml (
Element(..),
Attribute(..),
ElementType(..),
RenderCallbackFn(..),
containsAttribute,
findAttribute,
getAttributes,
getChildren,
parseXmlFile,
render,
render'
) where

import Data.String.Utils
import Data.List (intercalate)
import Text.Regex
import qualified Data.Map as Map


data Attribute = Attribute { attname :: String, attvalue :: String, occurrence :: Int }
               | NoAttribute
                 deriving (Show)

data ElementType = Root
                 | Raw
                 | Open
                 | Closed
                 deriving (Show)

data Element = Element ElementType String [Attribute] [Element]
               deriving (Show)

data RenderCallbackFn a b = RenderCallbackFn a (b -> RenderCallbackFn a b)


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


findAttribute :: String -> [Attribute] -> Attribute
findAttribute name [] = NoAttribute
findAttribute name (x:xs) = do
    let aname = attname x
    if aname == name then x
    else findAttribute name xs


containsAttribute :: String -> [Attribute] -> Bool
containsAttribute name [] = False
containsAttribute name (x:xs) = do
    let aname = attname x
    if aname == name then True
    else containsAttribute name xs


getChildren :: Element -> [Element]
getChildren (Element elemtype s atts xs) = xs


getAttributes :: Element -> [Attribute]
getAttributes (Element elemtype s atts xs) = atts


-- todo: fix escaped double quote in attr value
parseAttributes          :: String -> Map.Map String Int -> ([Attribute], Map.Map String Int)
parseAttributes "" dm    = ([], dm)
parseAttributes ">" dm   = ([], dm)
parseAttributes " />" dm = ([], dm)
parseAttributes "/>" dm  = ([], dm)
parseAttributes s dm     = do
    let news = dropWhile (matches [' ', '"']) s
    let (name, maybeValue) = splitOn '=' news
    let (value, rest) = span (not . matches ['"']) $ dropWhile (matches ['=', '"', '>']) maybeValue
    let key = name ++ "=" ++ value
    let count = Map.findWithDefault 0 key dm
    let newdm = Map.insert key (count+1) dm
    let (newatts, finaldm) = if rest /= "" then parseAttributes (tail rest) newdm else ([], newdm)
    ([Attribute name value (count+1)] ++ newatts, finaldm)


-- return the tag name, and then the remaining content of the element
parseTag :: String -> (String, String)
parseTag s = do
    let (_, remainder) = span (matches ['<','/']) s
    span (not . matches [' ','>', '/']) remainder


-- internal xml parser code
parse :: [String] -> Map.Map String Int -> ([Element], [String], Map.Map String Int)
parse [] dm = ([], [], dm)
parse (x:xs) dm = do
    let first = head x
    let sec = head (tail x)
    let seclst = last (init x)
    let lst = last x
    
    case (first, sec, seclst, lst) of
        ('<', '?', _, _)   -> do
            let (parsed, remaining, dm1) = parse xs dm
            ([Element Raw x [] []] ++ parsed, remaining, dm1)
        ('<', '!', _, _)   -> do
            let (parsed, remaining, dm1) = parse xs dm
            ([Element Raw x [] []] ++ parsed, remaining, dm1)
        ('<', _, '/', '>') -> do
            let (tag, tagcontent) = parseTag x
            let (attributes, dm1) = parseAttributes tagcontent dm
            let (parsed, remaining, dm2) = parse xs dm1
            ([Element Closed tag attributes []] ++ parsed, remaining, dm2)
        ('<', '/', _, '>') -> ([], xs, dm)
        ('<', _, _, '>')   -> do
            let (tag, tagcontent) = parseTag x
            let (attributes, dm1) = parseAttributes tagcontent dm
            let (children, siblings, dm2) = parse xs dm1
            let (parsed, remaining, dm3) = parse siblings dm2
            ([Element Open tag attributes children] ++ parsed, remaining, dm3)
        (_, _, _, _)       -> do
            let (parsed, remaining, dm1) = parse xs dm
            ([Element Raw x [] []] ++ parsed, remaining, dm1)


parseXmlFile :: String -> IO Element
parseXmlFile fname = do    
   file <- readFile fname
   let sp = splitText file
   let (parsed, _, _) = parse sp Map.empty
   return (Element Root "" [] parsed)


getData (RenderCallbackFn a b) = do
    let (tag, atts, xs) = a
    (tag, atts, xs)


getFn (RenderCallbackFn a b) = b


--renderNoop :: String -> [Attribute] -> [Element] -> (String, [Attribute], [Element])
renderNoop (s, atts, xs) = RenderCallbackFn (s, atts, xs) renderNoop


render :: Element -> String
render el = render' el renderNoop


render' (Element elemtype s atts xs) fn = do
    case elemtype of
        (Raw) -> s
        (Closed) -> renderClosed s atts fn
        (Open) -> renderOpen s atts xs fn
        (Root) -> renderList xs fn


--renderClosed :: RenderCallbackFn a b -> String
renderClosed s atts fn = do
    let fnres = fn (s, atts, [(Element Raw "" [] [])])
    let (newtag, newatts, _) = getData fnres
    "<" ++ newtag ++ (renderAttributeList newatts) ++ " />"


--renderOpen :: String -> [Attribute] -> [Element] -> (String -> [Attribute] -> [Element] -> (String, [Attribute], [Element])) -> String
renderOpen s atts xs fn = do
    let fnres = fn (s, atts, xs)
    let (newtag, newatts, newxs) = getData fnres
    let newfn = getFn fnres
    "<" ++ newtag ++ (renderAttributeList newatts) ++ ">" ++ (renderList newxs newfn) ++ "</" ++ newtag ++ ">"


--renderList :: [Element] -> (String -> [Attribute] -> [Element] -> (String, [Attribute], [Element])) -> String
renderList [] fn     = ""
renderList (x:xs) fn = do
    (render' x fn) ++ (intercalate "" (map renderInternal xs))
    where renderInternal x = render' x fn


renderAttribute :: Attribute -> String
renderAttribute (Attribute name val occ) = " " ++ name ++ "=\"" ++ val ++ "\""


renderAttributeList :: [Attribute] -> String
renderAttributeList [] = ""
renderAttributeList (x:xs) = renderAttribute x ++ (intercalate "" (map renderAttribute xs))
