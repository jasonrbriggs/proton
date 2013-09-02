module Proton.Template 
where

    {-
    (
    Template(..),
    setElementValue,
    setElementValues,
    setAttributeValue,
    repeatElement,
    hideElement,
    loadTemplates,
    getTemplate,
    renderTemplate
    ) 
    -}


import qualified Data.Map as Map
import Data.Maybe as Mb
import System.Directory
import System.FilePath
import Data.String.Utils

import Debug.Trace

import Proton.Xml as Xml
import Proton.XmlTypes as XmlTypes


data DataValue = DataValue { dval :: String, dpos :: Integer }
               | DataNameValue { dnname :: String, dnval :: String, dnpos :: Integer }
               | Repeat Integer
               | Hide Integer
                deriving (Show)


data DataMap = DataMap { eid_map :: (Map.Map String [DataValue]), aid_map :: (Map.Map String [DataValue]) }

instance Show DataMap where
    show (DataMap eid_map aid_map) = "DataMap: " ++ (show eid_map) ++ "," ++ (show aid_map)


data Template = Template { xml :: XmlTypes.Element, data_map :: DataMap }
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
    tmp <- repeatElement tmp eid 0 (toInteger $ length values)
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


repeatElement tmp rid pos count = do
    let (Element elemtype s atts xs) = xml tmp
    let dm = data_map tmp
    let (newxs, current) = (repeatElements rid pos 0 count xs)
    return $ Template (Element elemtype s atts newxs) dm


repeatElements :: String -> Integer -> Integer -> Integer -> [Element] -> ([Element], Integer)
repeatElements _ _ current _ [] = ([], current)
repeatElements rid pos current count (x:xs) = do
    let (xs1, newcurrent) = (repeatElement' x rid pos current count)
    let (xs2, newcurrent2) = (repeatElements rid pos newcurrent count xs) 
    (xs1 ++ xs2, newcurrent2)


repeatElement' :: Element -> String -> Integer -> Integer -> Integer -> ([Element], Integer)
repeatElement' x rid pos current count = do
    let (Element elemtype s atts xs) = x
    let ridatt = findAttribute "rid" atts
    case ridatt of
        (Attribute name val occ) -> do
            if val == rid
                then do
                    let newcurrent = current + 1
                    if newcurrent == pos || pos <= 0
                        then ([x] ++ repeatElementCopy x (count - 1), newcurrent)
                        else do
                            let (newxs, newcurrent2) = (repeatElements rid pos newcurrent count xs)
                            ([Element elemtype s atts newxs], newcurrent2)
                else do
                    let (newxs, newcurrent) = (repeatElements rid pos current count xs)
                    ([Element elemtype s atts newxs], newcurrent)
        (NoAttribute) -> do
            let (newxs, newcurrent) = (repeatElements rid pos current count xs)
            ([Element elemtype s atts newxs], newcurrent)


repeatElementCopy x 0 = []
repeatElementCopy x count =
    [copyElement x] ++ repeatElementCopy x (count - 1)


hideElement tmp eid pos = do
    let (Element elemtype s atts xs) = xml tmp
    let dm = data_map tmp
    let (newxs, current) = (hideElements eid pos 0 xs)
    return $ Template (Element elemtype s atts newxs) dm


hideElements eid pos current [] = ([], current)
hideElements eid pos current (e:es) = do
    let (Element elemtype s atts xs) = e
    let eidatt = findAttribute "eid" atts
    case eidatt of
        (Attribute name val occ) -> do
            if val == eid
                then do
                    let newcurrent = current + 1
                    if newcurrent == pos || pos <= 0
                        then (es, current)
                        else hideElement' eid pos newcurrent (e:es)
                else hideElement' eid pos current (e:es)
        (NoAttribute) -> hideElement' eid pos current (e:es)


hideElement' eid pos current [] = ([], current)
hideElement' eid pos current (e:es) = do
     let (Element elemtype s atts xs) = e
     let (newxs, newcurrent) = hideElements eid pos current xs
     let (newxs2, newcurrent2) = hideElements eid pos newcurrent es
     ([Element elemtype s atts newxs] ++ newxs2, newcurrent2)


--renderReplace :: DataMap -> Map.Map String Integer -> (String, [Attribute], [Element]) -> RenderCallbackFn (String, [Attribute], [Element]) (String, [Attribute], [Element])
renderReplace dm (s, atts, xs) = do
    let newxs = if containsAttribute "eid" atts 
        then do
            let eidatt = findAttribute "eid" atts
            renderReplaceEID dm xs eidatt
        else xs

    let newatts = if containsAttribute "aid" atts 
        then do
            let aidatt = findAttribute "aid" atts
            renderReplaceAID dm atts aidatt
        else atts

    RenderCallbackFn (s, newatts, newxs) (renderReplace dm)


--renderReplaceEID :: DataMap -> Integer -> [Element] -> Attribute -> [Element]
renderReplaceEID dm xs eidatt = do
    let emap = eid_map dm
    let vs = Map.findWithDefault [] (attvalue eidatt) emap
    let occurrence = occ eidatt
    renderReplaceEID' occurrence xs vs


renderReplaceEID' occurrence xs [] = xs
renderReplaceEID' occurrence xs (e:es) = do
    let pos = dpos e
    if occurrence == pos || pos <= 0
        then [Element Raw (dval e) [] []]
        else renderReplaceEID' occurrence xs es


renderReplaceAID :: DataMap -> [Attribute] -> Attribute -> [Attribute]
renderReplaceAID dm atts aidatt = do
    let amap = aid_map dm
    let as = Map.findWithDefault [] (attvalue aidatt) amap
    let occurrence = occ aidatt
    renderReplaceAID' occurrence atts as


renderReplaceAID' occurrence atts [] = atts
renderReplaceAID' occurrence atts (a:as) = do
    let pos = dnpos a
    let name = dnname a
    let value = dnval a
    if occurrence == pos || pos <= 0
        then replaceAttributeValue name value atts
        else renderReplaceAID' occurrence atts as


replaceAttributeValue :: String -> String -> [Attribute] -> [Attribute]
replaceAttributeValue name newvalue [] = []
replaceAttributeValue name newvalue (a:as) = do
    let aname = attname a
    let occurrence = occ a
    if name == aname 
        then [Attribute name newvalue occurrence] ++ as
        else [a] ++ replaceAttributeValue name newvalue as


renderTemplate tmp = do
    let dm = data_map tmp
    let x = xml tmp
    let renderReplaceInternal = renderReplace dm
    return (render' x renderReplaceInternal)
