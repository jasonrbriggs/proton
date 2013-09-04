module Proton.Xml (
Element(..),
Attribute(..),
ElementType(..),
RenderCallbackFn(..),
containsAttribute,
copyElement,
copyElements,
findAttribute,
getAttributes,
getChildren,
parseXmlFile,
parseAttributes,
render,
render'
) where


import Data.List (intercalate)
import qualified Data.Map as Map

import Proton.XmlTypes
import Proton.XmlInternal


containsAttribute :: String -> [Attribute] -> Bool
containsAttribute name [] = False
containsAttribute name (x:xs) = do
    let aname = attname x
    if aname == name then True
    else containsAttribute name xs


copyElement :: Element -> Element
copyElement (Element elemtype s atts xs) = Element elemtype s atts (copyElements xs)


copyElements :: [Element] -> [Element]
copyElements [] = []
copyElements (x:xs) = do
    [copyElement x] ++ (copyElements xs)


findAttribute :: String -> [Attribute] -> Attribute
findAttribute name [] = NoAttribute
findAttribute name (x:xs) = do
    let aname = attname x
    if aname == name then x
    else findAttribute name xs


getChildren :: Element -> [Element]
getChildren (Element elemtype s atts xs) = xs


getAttributes :: Element -> [Attribute]
getAttributes (Element elemtype s atts xs) = atts


-- parse a string into a list of attributes
parseAttributes       :: String -> [Attribute]
parseAttributes ""    = []
parseAttributes ">"   = []
parseAttributes " />" = []
parseAttributes "/>"  = []
parseAttributes s     = do
    let news = dropWhile (matches [' ', '"']) s
    let (name, maybeValue) = splitOn '=' news
    let (value, rest) = splitUntilClose maybeValue
    [Attribute name value 1] ++ (if rest /= "" then parseAttributes (tail rest) else [])


-- return the tag name, and then the remaining content of the element
parseTag   :: String -> (String, String)
parseTag s = do
    let (_, remainder) = span (matches ['<','/']) s
    span (not . matches [' ','>', '/']) remainder


-- internal xml parser code
parse    :: [String] -> ([Element], [String])
parse [] = ([], [])
parse (x:xs) = do
    let first = head x
    let sec = head (tail x)
    let seclst = last (init x)
    let lst = last x
    
    case (first, sec, seclst, lst) of
        ('<', '?', _, _)   -> do
            let (parsed, remaining) = parse xs
            ([Element Raw x [] []] ++ parsed, remaining)
        ('<', '!', _, _)   -> do
            let (parsed, remaining) = parse xs
            ([Element Raw x [] []] ++ parsed, remaining)
        ('<', _, '/', '>') -> do
            let (tag, tagcontent) = parseTag x
            let attributes = parseAttributes tagcontent
            let (parsed, remaining) = parse xs
            ([Element Closed tag attributes []] ++ parsed, remaining)
        ('<', '/', _, '>') -> ([], xs)
        ('<', _, _, '>')   -> do
            let (tag, tagcontent) = parseTag x
            let attributes = parseAttributes tagcontent
            let (children, siblings) = parse xs
            let (parsed, remaining) = parse siblings
            ([Element Open tag attributes children] ++ parsed, remaining)
        (_, _, _, _)       -> do
            let (parsed, remaining) = parse xs
            ([Element Raw x [] []] ++ parsed, remaining)


parseXmlFile       :: String -> IO Element
parseXmlFile fname = do    
   file <- readFile fname
   let sp = splitText file
   let (parsed, _) = parse sp
   return (Element Root "" [] parsed)


getData (RenderCallbackFn a b) = do
    let (tag, atts, xs) = a
    (tag, atts, xs)


getFn (RenderCallbackFn a b) = b


-- the "no op" function for basic rendering (i.e. render without callback)
renderNoop :: (String, [Attribute], [Element]) -> RenderCallbackFn (String, [Attribute], [Element]) (String, [Attribute], [Element])
renderNoop (s, atts, xs) = RenderCallbackFn (s, atts, xs) renderNoop


render   :: Element -> String
render e = render' e renderNoop


render' :: Element -> ((String, [Attribute], [Element]) -> RenderCallbackFn (String, [Attribute], [Element]) (String, [Attribute], [Element])) -> String
render' e fn = do
    let (newe, _) = preprocessElement e Map.empty
    renderElement newe fn


incrementOccurrences :: [Attribute] -> Map.Map String Integer -> ([Attribute], Map.Map String Integer)
incrementOccurrences [] occurrences     = ([], occurrences)
incrementOccurrences (a:as) occurrences = do
    let (Attribute name val occurrence) = a

    if name == "eid" || name == "aid"
        then do
            let key = name ++ "/" ++ val
            let count = (Map.findWithDefault 0 key occurrences) + 1
            let newoccurrences = Map.insert key count occurrences
            let (newatts, newoccurrences2) = incrementOccurrences as newoccurrences
            ([Attribute name val count] ++ newatts, newoccurrences2)
        else do
            let (newatts, newoccurrences) = incrementOccurrences as occurrences
            ([a] ++ newatts, newoccurrences)


preprocessElement :: Element -> Map.Map String Integer -> (Element, Map.Map String Integer)
preprocessElement e occurrences  = do
    let (Element elemtype s atts xs) = e
    let (newatts, newoccurrences) = incrementOccurrences atts occurrences
    let (newxs, newoccurrences2) = preprocessElement' xs newoccurrences
    (Element elemtype s newatts newxs, newoccurrences2)


preprocessElement' :: [Element] -> Map.Map String Integer -> ([Element], Map.Map String Integer)
preprocessElement' [] occurrences = ([], occurrences)
preprocessElement' (e:es) occurrences = do
    let (newe, newoccurrences) = preprocessElement e occurrences
    let (newes, newoccurrences2) = preprocessElement' es newoccurrences
    ([newe] ++ newes, newoccurrences2)
    

renderElement :: Element -> ((String, [Attribute], [Element]) -> RenderCallbackFn (String, [Attribute], [Element]) (String, [Attribute], [Element])) -> String
renderElement (Element elemtype s atts xs) fn = do
    case elemtype of
        (Raw) -> s
        (Closed) -> renderClosed s atts fn
        (Open) -> renderOpen s atts xs fn
        (Root) -> renderList xs fn


renderClosed :: String -> [Attribute] -> ((String, [Attribute], [Element]) -> RenderCallbackFn (String, [Attribute], [Element]) (String, [Attribute], [Element])) -> String
renderClosed s atts fn = do
    let fnres = fn (s, atts, [(Element Raw "" [] [])])
    let (newtag, newatts, _) = getData fnres
    "<" ++ newtag ++ (renderAttributeList newatts) ++ " />"


renderOpen :: String -> [Attribute] -> [Element] -> ((String, [Attribute], [Element]) -> RenderCallbackFn (String, [Attribute], [Element]) (String, [Attribute], [Element])) -> String
renderOpen s atts xs fn = do
    let fnres = fn (s, atts, xs)
    let (newtag, newatts, newxs) = getData fnres
    let newfn = getFn fnres
    "<" ++ newtag ++ (renderAttributeList newatts) ++ ">" ++ (renderList newxs newfn) ++ "</" ++ newtag ++ ">"


renderList :: [Element] -> ((String, [Attribute], [Element]) -> RenderCallbackFn (String, [Attribute], [Element]) (String, [Attribute], [Element])) -> String
renderList [] fn     = ""
renderList (x:xs) fn = (renderElement x fn) ++ (renderList xs fn)


renderAttribute :: Attribute -> String
renderAttribute (Attribute name val occ) = do
    if name == "rid" || name == "eid" || name == "aid" 
        then ""
        else " " ++ name ++ "=\"" ++ val ++ "\""


renderAttributeList    :: [Attribute] -> String
renderAttributeList [] = ""
renderAttributeList (x:xs) = renderAttribute x ++ renderAttributeList xs
