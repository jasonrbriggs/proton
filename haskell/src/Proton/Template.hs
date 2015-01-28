module Proton.Template (
Template(..),
setElementValue,
setElementValues,
setAttributeValue,
repeatElement,
hideElement,
loadTemplates,
getTemplate,
include,
renderTemplate
) where

import qualified Data.Map as Map
import Data.Maybe as Mb
import System.Directory
import System.FilePath
import Data.List

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


data Template = Template { xml :: XmlTypes.Element, data_map :: DataMap, tmpsref :: Templates }
              | NoTemplate


instance Show Template where
  show (Template xml data_map tmps) = "Template: " ++ (show data_map)
  show (NoTemplate) = "NoTemplate"
  
  
data Templates = Templates { tmpl_map :: (Map.Map String Template) }
               | DummyTemplates
    deriving (Show)


validExt :: String -> Bool
validExt f = (isSuffixOf "xhtml" f) || (isSuffixOf "xml" f)


getValidFiles :: FilePath -> IO [String]
getValidFiles dir = do
    d <- getDirectoryContents dir
    return (map (\x -> dir ++ [pathSeparator] ++ x) (filter (validExt) d))


loadTemplates :: String -> IO Templates
loadTemplates dir = do
    let tmps = Templates Map.empty
    dircontents <- getValidFiles dir
    loadTemplates' tmps dircontents


loadTemplates' :: Templates -> [String] -> IO Templates
loadTemplates' tmps [] = return tmps
loadTemplates' tmps (s:ss) = do
    tmps <- loadTemplate tmps s
    loadTemplates' tmps ss


loadTemplate :: Templates -> String -> IO Templates
loadTemplate tmps name = do
    if (Map.member name (tmpl_map tmps)) then return tmps
    else do
        x <- parseXmlFile name
        let t = Template x (DataMap Map.empty Map.empty) DummyTemplates
        let templateMap = tmpl_map tmps
        return tmps { tmpl_map = Map.insert name t templateMap }


getTemplate  :: Templates -> String -> IO Template
getTemplate tmps name = do
    let mp = tmpl_map tmps
    let (Template x dm _) = Map.findWithDefault NoTemplate name mp
    return (Template x dm tmps)


setElementValue :: Template -> String -> String -> Integer -> IO Template
setElementValue tmp eid value pos = do
    let dm = data_map tmp
    let em = eid_map dm
    let am = aid_map dm
    let x = xml tmp
    let tmps = tmpsref tmp
    let elemlist = Map.findWithDefault [] eid em
    let newem = Map.insert eid (elemlist ++ [DataValue value pos]) em
    let newdm = DataMap newem am
    return (Template x newdm tmps)


setElementValues :: Template -> String -> [String] -> IO Template
setElementValues tmp eid values = do
    tmp <- repeatElement tmp eid 0 (toInteger $ length values)
    setElementValues' tmp eid values 1


setElementValues' :: Template -> String -> [String] -> Integer -> IO Template
setElementValues' tmp _ [] _ = return tmp
setElementValues' tmp eid (s:ss) pos = do
    tmp <- setElementValue tmp eid s pos
    setElementValues' tmp eid ss (pos + 1)


setAttributeValue :: Template -> String -> String -> String -> Integer -> IO Template
setAttributeValue tmp aid attname value pos = do
    let dm = data_map tmp
    let em = eid_map dm
    let am = aid_map dm
    let x = xml tmp
    let tmps = tmpsref tmp
    let attlist = Map.findWithDefault [] aid am
    let newam = Map.insert aid (attlist ++ [DataNameValue attname value pos]) am
    let newdm = DataMap em newam
    return (Template x newdm tmps)


include :: Template -> String -> String -> Integer -> IO Template
include tmp eid template_name pos = do
    let (Element elemtype s atts xs) = xml tmp
    let dm = data_map tmp
    let tmps = tmpsref tmp
    included_tmp <- getTemplate tmps template_name
    case included_tmp of
        NoTemplate -> return tmp
        _          -> do
                let (Element _ _ _ include_xs) = xml included_tmp
                let (newxs, _) = (includeSearch' eid include_xs pos 0 xs)
                return $ Template (Element elemtype s atts newxs) dm tmps


include' :: Element -> String -> [Element] -> Integer -> Integer -> ([Element], Integer)
include' x eid include_xs pos current = do
    let (Element elemtype s atts xs) = x
    let eidatt = findAttribute "eid" atts
    case eidatt of
        (Attribute name val occ) -> do
            if val == eid
                then do
                    let newcurrent = current + 1
                    if newcurrent == pos || pos <= 0
                        then (include_xs, newcurrent)
                        else do
                            let (newxs, newcurrent2) = (includeSearch' eid include_xs pos newcurrent xs)
                            ([Element elemtype s atts newxs], newcurrent2)
                else do
                    let (newxs, newcurrent) = (includeSearch' eid include_xs pos current xs)
                    ([Element elemtype s atts newxs], newcurrent)
        (NoAttribute) -> do
            let (newxs, newcurrent) = (includeSearch' eid include_xs pos current xs)
            ([Element elemtype s atts newxs], newcurrent)


includeSearch' :: String -> [Element] -> Integer -> Integer -> [Element] -> ([Element], Integer)
includeSearch' eid include_xs pos current [] = ([], current)
includeSearch' eid include_xs pos current (x:xs) = do
    let (xs1, newcurrent) = include' x eid include_xs pos current
    let (xs2, newcurrent2) = includeSearch' eid include_xs pos newcurrent xs
    (xs1 ++ xs2, newcurrent2)


repeatElement :: Template -> String -> Integer -> Integer -> IO Template
repeatElement tmp rid pos count = do
    let (Element elemtype s atts xs) = xml tmp
    let dm = data_map tmp
    let tmps = tmpsref tmp
    let (newxs, current) = (repeatElements rid pos 0 count xs)
    return $ Template (Element elemtype s atts newxs) dm tmps


repeatElements :: String -> Integer -> Integer -> Integer -> [Element] -> ([Element], Integer)
repeatElements _ _ current _ [] = ([], current)
repeatElements rid pos current count (x:xs) = do
    let (xs1, newcurrent) = repeatElement' x rid pos current count
    let (xs2, newcurrent2) = repeatElements rid pos newcurrent count xs
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


repeatElementCopy :: Element -> Integer -> [Element]
repeatElementCopy x 0 = []
repeatElementCopy x count =
    [copyElement x] ++ repeatElementCopy x (count - 1)


hideElement :: Template -> String -> Integer -> IO Template
hideElement tmp eid pos = do
    let (Element elemtype s atts xs) = xml tmp
    let dm = data_map tmp
    let tmps = tmpsref tmp
    let (newxs, current) = (hideElements eid pos 0 xs)
    return $ Template (Element elemtype s atts newxs) dm tmps


hideElements :: String -> Integer -> Integer -> [Element] -> ([Element], Integer)
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


hideElement' :: String -> Integer -> Integer -> [Element] -> ([Element], Integer)
hideElement' eid pos current [] = ([], current)
hideElement' eid pos current (e:es) = do
     let (Element elemtype s atts xs) = e
     let (newxs, newcurrent) = hideElements eid pos current xs
     let (newxs2, newcurrent2) = hideElements eid pos newcurrent es
     ([Element elemtype s atts newxs] ++ newxs2, newcurrent2)


renderReplace :: DataMap -> (String, [Attribute], [Element]) -> RenderCallbackFn (String, [Attribute], [Element]) (String, [Attribute], [Element])
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


renderReplaceEID :: DataMap -> [Element] -> Attribute -> [Element]
renderReplaceEID dm xs eidatt = do
    let emap = eid_map dm
    let vs = Map.findWithDefault [] (attvalue eidatt) emap
    let occurrence = occ eidatt
    renderReplaceEID' occurrence xs vs


renderReplaceEID' :: Integer -> [Element] -> [DataValue] -> [Element]
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


renderReplaceAID' :: Integer -> [Attribute] -> [DataValue] -> [Attribute]
renderReplaceAID' occurrence atts [] = atts
renderReplaceAID' occurrence atts (a:as) = do
    let pos = dnpos a
    let name = dnname a
    let value = dnval a
    if occurrence == pos || pos <= 0
        then do
            let newatts = replaceAttributeValue name value atts
            renderReplaceAID' occurrence newatts as
        else renderReplaceAID' occurrence atts as


replaceAttributeValue :: String -> String -> [Attribute] -> [Attribute]
replaceAttributeValue name newvalue [] = [Attribute name newvalue 1]
replaceAttributeValue name newvalue (a:as) = do
    let aname = attname a
    let occurrence = occ a
    if name == aname 
        then [Attribute name newvalue occurrence] ++ as
        else [a] ++ replaceAttributeValue name newvalue as


renderTemplate :: Template -> IO String
renderTemplate tmp = do
    let dm = data_map tmp
    let x = xml tmp
    let renderReplaceInternal = renderReplace dm
    return (render' x renderReplaceInternal)
