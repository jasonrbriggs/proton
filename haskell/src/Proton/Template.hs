module Proton.Template (
Template(..),
setElementValue,
setElementValues,
setAttributeValue,
repeatElement,
hideElement,
loadTemplates,
getTemplate,
renderTemplate
) where

import qualified Data.Map as Map
import Data.Maybe as Mb
import System.Directory
import System.FilePath
import Data.String.Utils

import Proton.Xml as Xml

data DataValue = DataValue { dval :: String, dpos :: Integer }
               | DataNameValue { dnname :: String, dnval :: String, dnpos :: Integer }
               | Repeat Integer
               | Hide Integer
                deriving (Show)


data DataMap = DataMap { eid_map :: (Map.Map String [DataValue]), aid_map :: (Map.Map String [DataValue]) }

instance Show DataMap where
    show (DataMap eid_map aid_map) = "DataMap: " ++ (show eid_map) ++ "," ++ (show aid_map)

data Template = Template { xml :: Xml.Element, data_map :: DataMap }
              | NoTemplate

instance Show Template where
  show (Template xml data_map) = "Template: " ++ (show data_map)
  show (NoTemplate) = "NoTemplate"
  
  
data Templates = Templates { tmpl_map :: (Map.Map String Template) }
    deriving (Show)


validExt f = (endswith "xhtml" f) || (endswith "xml" f)

getValidFiles dir = do
    d <- getDirectoryContents dir
    return (map (\x -> dir ++ [pathSeparator] ++ x) (filter (validExt) d))


loadTemplates dir = do
    let tmps = Templates Map.empty
    dircontents <- getValidFiles dir
    loadTemplates' tmps dircontents


loadTemplates' tmps [] = return tmps
loadTemplates' tmps (x:xs) = do
    tmps <- loadTemplate tmps x
    loadTemplates' tmps xs


loadTemplate tmps name = do
    if (Map.member name (tmpl_map tmps)) then return tmps
    else do
        x <- parseXmlFile name
        let t = Template x (DataMap Map.empty Map.empty)
        let templateMap = tmpl_map tmps
        return tmps { tmpl_map = Map.insert name t templateMap }


--getTemplate  :: Templates -> String -> IO Template
getTemplate tmps name = do
    let mp = tmpl_map tmps
    return (Map.findWithDefault NoTemplate name mp)


setElementValue tmp eid value pos = do
    let dm = data_map tmp
    let em = eid_map dm
    let am = aid_map dm
    let x = xml tmp
    let elemlist = Map.findWithDefault [] eid em
    let newem = Map.insert eid (elemlist ++ [DataValue value pos]) em
    let newdm = DataMap newem am
    return (Template x newdm)

setElementValues tmp eid values = do
    tmp <- repeatElement tmp eid (toInteger $ length values)
    setElementValues' tmp eid values 1

setElementValues' tmp _ [] _ = return tmp
setElementValues' tmp eid (x:xs) pos = do
    tmp <- setElementValue tmp eid x pos
    setElementValues' tmp eid xs (pos + 1)


setAttributeValue tmp aid attname value pos = do
    let dm = data_map tmp
    let em = eid_map dm
    let am = aid_map dm
    let x = xml tmp
    let attlist = Map.findWithDefault [] aid am
    let newam = Map.insert aid (attlist ++ [DataNameValue attname value pos]) am
    let newdm = DataMap em newam
    return (Template x newdm)


repeatElement tmp rid count = do
    let (Element elemtype s atts xs) = xml tmp
    let dm = data_map tmp
    return $ Template (Element elemtype s atts (repeatElements xs rid count)) dm


repeatElements :: [Element] -> String -> Integer -> [Element]
repeatElements [] _ _ = []
repeatElements (x:xs) rid count = (repeatElement' x rid count) ++ (repeatElements xs rid count)


repeatElement' :: Element -> String -> Integer -> [Element]
repeatElement' x rid count = do
    let (Element elemtype s atts xs) = x
    let ridatt = findAttribute "rid" atts
    case ridatt of
        (Attribute name val occ) -> do
            if val == rid
                then do
                    [x] ++ repeatElementCopy x (count - 1) 1
                else [Element elemtype s atts (repeatElements xs rid count)]
        (NoAttribute) -> [Element elemtype s atts (repeatElements xs rid count)]


repeatElementCopy x 0 _ = []
repeatElementCopy x count increment =
    [copyElement x increment] ++ repeatElementCopy x (count - 1) (increment + 1)


hideElement tmp eid pos = do
    let (Element elemtype s atts xs) = xml tmp
    let dm = data_map tmp
    return $ Template (Element elemtype s atts (hideElements eid pos xs)) dm


hideElements eid pos [] = []
hideElements eid pos (e:es) = do
    let (Element elemtype s atts xs) = e
    let eidatt = findAttribute "eid" atts
    case eidatt of
        (Attribute name val occ) -> do
            if val == eid || pos == occ
                then es
                else [hideElement' eid pos e] ++ hideElements eid pos es
        (NoAttribute) -> [hideElement' eid pos e] ++ hideElements eid pos es


hideElement' eid pos e = do
     let (Element elemtype s atts xs) = e
     Element elemtype s atts (hideElements eid pos xs)


renderReplace dm (s, atts, xs) = do
    let newxs = if containsAttribute "eid" atts then renderReplaceEID dm xs (findAttribute "eid" atts) else xs
    -- todo
    let newatts = if containsAttribute "aid" atts then renderReplaceAID dm atts (findAttribute "aid" atts) else atts
    
    RenderCallbackFn (s, newatts, newxs) (renderReplace dm)


renderReplaceEID dm xs eidatt = do
    let emap = eid_map dm
    let vs = Map.findWithDefault [] (attvalue eidatt) emap
    renderReplaceEID' (occurrence eidatt) xs vs


renderReplaceEID' attocc xs [] = xs
renderReplaceEID' attocc xs (e:es) = do
    let pos = dpos e
    if attocc == pos || pos <= 0
        then [Element Raw (dval e) [] []]
        else renderReplaceEID' attocc xs es


renderReplaceAID dm atts aidatt = do
    let amap = aid_map dm
    let as = Map.findWithDefault [] (attvalue aidatt) amap
    renderReplaceAID' (occurrence aidatt) atts as


renderReplaceAID' attocc atts [] = atts
renderReplaceAID' attocc atts (a:as) = do
    let pos = dnpos a
    let name = dnname a
    let value = dnval a
    if attocc == pos || pos <= 0
        then replaceAttributeValue name value atts
        else renderReplaceAID' attocc atts as


replaceAttributeValue :: String -> String -> [Attribute] -> [Attribute]
replaceAttributeValue name newvalue [] = []
replaceAttributeValue name newvalue (a:as) = do
    let aname = attname a
    let aocc = occurrence a
    if name == aname 
        then [Attribute name newvalue aocc] ++ as
        else [a] ++ replaceAttributeValue name newvalue as


renderTemplate tmp = do
    let dm = data_map tmp
    let x = xml tmp
    let renderReplaceInternal = renderReplace dm
    let s = render' x renderReplaceInternal
    return s



{-
renderWithReplacement :: (Map String String) -> String -> [Attribute] -> [Element] -> (String, [Attribute], [Element])
renderWithReplacement dm s as xs = (s, as, xs)
-}
