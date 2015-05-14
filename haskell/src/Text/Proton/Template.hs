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

module Text.Proton.Template (
Template(..),
Templates(..),
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
import System.Directory
import System.FilePath
import Data.List

import Text.Proton.Xml as Xml
import Text.Proton.XmlTypes as XmlTypes


-- | A data value is used to populate an element/attribute in an xml document
-- It denotes either an element value (string) and the index position to set, or an attribute name, with value and index position,
-- or the number of times to repeat an element, or to hide an element
data DataValue = DataValue { dval :: String, dpos :: Integer }
               | DataNameValue { dnname :: String, dnval :: String, dnpos :: Integer }
               | Repeat Integer
               | Hide Integer
                deriving (Show)


-- | A map of element ids to DataValues, and attribute ids to DataValues
data DataMap = DataMap { eidMap :: Map.Map String [DataValue], aidMap :: Map.Map String [DataValue] }


instance Show DataMap where
    show (DataMap eid aid) = "DataMap: " ++ show eid ++ "," ++ show aid


-- | A template is described by a root xml element, a DataMap, and the reference to the Templates instance
data Template = Template { xml :: XmlTypes.Element, dataMap :: DataMap, tmpsref :: Templates }
              | NoTemplate


instance Show Template where
  show (Template _ dm _) = "Template: " ++ show dm
  show (NoTemplate) = "NoTemplate"
  

-- | Templates comprises a map of file names to Template instances
data Templates = Templates { dir :: String, tmplMap :: Map.Map String Template }
               | DummyTemplates
    deriving (Show)


validExt :: String -> Bool
validExt f = isSuffixOf "xhtml" f || isSuffixOf "xml" f


getValidFiles :: FilePath -> IO [String]
getValidFiles path = do
    d <- getDirectoryContents path
    return (map (\x -> x) (filter validExt d))


loadTemplates :: String -> IO Templates
loadTemplates path = do
    let tmps = Templates path Map.empty
    dircontents <- getValidFiles path
    loadTemplates' tmps dircontents


loadTemplates' :: Templates -> [String] -> IO Templates
loadTemplates' tmps [] = return tmps
loadTemplates' tmps (s:ss) = do
    newTmps <- loadTemplate tmps s
    loadTemplates' newTmps ss


loadTemplate :: Templates -> String -> IO Templates
loadTemplate tmps name =
    if Map.member name (tmplMap tmps) 
        then return tmps
        else do
            let d = dir tmps
            x <- parseXmlFile $ d ++ [pathSeparator] ++ name
            let t = Template x (DataMap Map.empty Map.empty) DummyTemplates
            let templateMap = tmplMap tmps
            return tmps { tmplMap = Map.insert name t templateMap }


getTemplate  :: Templates -> String -> IO Template
getTemplate tmps name = do
    let mp = tmplMap tmps
    let (Template x dm _) = Map.findWithDefault NoTemplate name mp
    return (Template x dm tmps)


setElementValue :: Template -> String -> String -> Integer -> IO Template
setElementValue tmp eid value pos = do
    let (_, em, am, x, tmps, _, _, _, _) = extractAttributes tmp
    let elemlist = Map.findWithDefault [] eid em
    let newem = Map.insert eid (elemlist ++ [DataValue value pos]) em
    let newdm = DataMap newem am
    return (Template x newdm tmps)


extractAttributes :: Template -> (DataMap, Map.Map String [DataValue], Map.Map String [DataValue], XmlTypes.Element, Templates, ElementType, String, [Attribute], [Element])
extractAttributes tmp = do
    let templateXml = xml tmp
    let dm = dataMap tmp
    let (Element elemtype s atts xs) = templateXml
    (dm, eidMap dm, aidMap dm, templateXml, tmpsref tmp, elemtype, s, atts, xs)


setElementValues :: Template -> String -> [String] -> IO Template
setElementValues tmp eid values = do
    newTmp <- repeatElement tmp eid 0 (toInteger $ length values)
    setElementValues' newTmp eid values 1


setElementValues' :: Template -> String -> [String] -> Integer -> IO Template
setElementValues' tmp _ [] _ = return tmp
setElementValues' tmp eid (s:ss) pos = do
    newTmp <- setElementValue tmp eid s pos
    setElementValues' newTmp eid ss (pos + 1)


setAttributeValue :: Template -> String -> String -> String -> Integer -> IO Template
setAttributeValue tmp aid att value pos = do
    let (_, em, am, x, tmps, _, _, _, _) = extractAttributes tmp
    let attlist = Map.findWithDefault [] aid am
    let newam = Map.insert aid (attlist ++ [DataNameValue att value pos]) am
    let newdm = DataMap em newam
    return (Template x newdm tmps)


include :: Template -> String -> String -> Integer -> IO Template
include tmp eid templateName pos = do
    let (dm, _, _, _, tmps, elemtype, s, atts, xs) = extractAttributes tmp
    includedTmp <- getTemplate tmps templateName
    case includedTmp of
        NoTemplate -> return tmp
        _          -> do
                let (Element _ _ _ include_xs) = xml includedTmp
                let (newxs, _) = includeSearch' eid include_xs pos 0 xs
                return $ Template (Element elemtype s atts newxs) dm tmps


include' :: Element -> String -> [Element] -> Integer -> Integer -> ([Element], Integer)
include' x eid include_xs pos current = do
    let (Element elemtype s atts xs) = x
    let eidatt = findAttribute "eid" atts
    case eidatt of
        (Attribute _ val _) ->
            if val == eid
                then do
                    let newcurrent = current + 1
                    if newcurrent == pos || pos <= 0
                        then (include_xs, newcurrent)
                        else do
                            let (newxs, newcurrent2) = includeSearch' eid include_xs pos newcurrent xs
                            ([Element elemtype s atts newxs], newcurrent2)
                else do
                    let (newxs, newcurrent) = includeSearch' eid include_xs pos current xs
                    ([Element elemtype s atts newxs], newcurrent)
        (NoAttribute) -> do
            let (newxs, newcurrent) = includeSearch' eid include_xs pos current xs
            ([Element elemtype s atts newxs], newcurrent)


includeSearch' :: String -> [Element] -> Integer -> Integer -> [Element] -> ([Element], Integer)
includeSearch' _ _ _ current [] = ([], current)
includeSearch' eid include_xs pos current (x:xs) = do
    let (xs1, newcurrent) = include' x eid include_xs pos current
    let (xs2, newcurrent2) = includeSearch' eid include_xs pos newcurrent xs
    (xs1 ++ xs2, newcurrent2)


repeatElement :: Template -> String -> Integer -> Integer -> IO Template
repeatElement tmp rid pos count = do
    let (dm, _, _, _, tmps, elemtype, s, atts, xs) = extractAttributes tmp
    let (newxs, _) = repeatElements rid pos 0 count xs
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
        (Attribute _ val _) ->
            if val == rid
                then do
                    let newcurrent = current + 1
                    if newcurrent == pos || pos <= 0
                        then (x : repeatElementCopy x (count - 1), newcurrent)
                        else do
                            let (newxs, newcurrent2) = repeatElements rid pos newcurrent count xs
                            ([Element elemtype s atts newxs], newcurrent2)
                else do
                    let (newxs, newcurrent) = repeatElements rid pos current count xs
                    ([Element elemtype s atts newxs], newcurrent)
        (NoAttribute) -> do
            let (newxs, newcurrent) = repeatElements rid pos current count xs
            ([Element elemtype s atts newxs], newcurrent)


repeatElementCopy :: Element -> Integer -> [Element]
repeatElementCopy _ 0 = []
repeatElementCopy x count =
    copyElement x : repeatElementCopy x (count - 1)


hideElement :: Template -> String -> Integer -> IO Template
hideElement tmp eid pos = do
    let (Element elemtype s atts xs) = xml tmp
    let dm = dataMap tmp
    let tmps = tmpsref tmp
    let (newxs, _) = hideElements eid pos 0 xs
    return $ Template (Element elemtype s atts newxs) dm tmps


hideElements :: String -> Integer -> Integer -> [Element] -> ([Element], Integer)
hideElements _ _ current [] = ([], current)
hideElements eid pos current (e:es) = do
    let (Element _ _ atts _) = e
    let eidatt = findAttribute "eid" atts
    case eidatt of
        (Attribute _ val _) ->
            if val == eid
                then do
                    let newcurrent = current + 1
                    if newcurrent == pos || pos <= 0
                        then (es, current)
                        else hideElement' eid pos newcurrent (e:es)
                else hideElement' eid pos current (e:es)
        (NoAttribute) -> hideElement' eid pos current (e:es)


hideElement' :: String -> Integer -> Integer -> [Element] -> ([Element], Integer)
hideElement' _ _ current [] = ([], current)
hideElement' eid pos current (e:es) = do
     let (Element elemtype s atts xs) = e
     let (newxs, newcurrent) = hideElements eid pos current xs
     let (newxs2, newcurrent2) = hideElements eid pos newcurrent es
     (Element elemtype s atts newxs : newxs2, newcurrent2)


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
    let emap = eidMap dm
    let vs = Map.findWithDefault [] (attvalue eidatt) emap
    let occurrence = occ eidatt
    renderReplaceEID' occurrence xs vs


renderReplaceEID' :: Integer -> [Element] -> [DataValue] -> [Element]
renderReplaceEID' _ xs [] = xs
renderReplaceEID' occurrence xs (e:es) = do
    let pos = dpos e
    if occurrence == pos || pos <= 0
        then [Element Raw (dval e) [] []]
        else renderReplaceEID' occurrence xs es


renderReplaceAID :: DataMap -> [Attribute] -> Attribute -> [Attribute]
renderReplaceAID dm atts aidatt = do
    let amap = aidMap dm
    let as = Map.findWithDefault [] (attvalue aidatt) amap
    let occurrence = occ aidatt
    renderReplaceAID' occurrence atts as


renderReplaceAID' :: Integer -> [Attribute] -> [DataValue] -> [Attribute]
renderReplaceAID' _ atts [] = atts
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
        then Attribute name newvalue occurrence : as
        else a : replaceAttributeValue name newvalue as


renderTemplate :: Template -> IO String
renderTemplate tmp = do
    let dm = dataMap tmp
    let x = xml tmp
    let renderReplaceInternal = renderReplace dm
    return (render' x renderReplaceInternal)
