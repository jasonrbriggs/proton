module Proton.Template where

import qualified Data.Map as Map
import Data.Maybe as Mb

import Proton.Xml as Xml

data DataValue = DataValue { dval :: String, dpos :: Int }
               | DataNameValue { dnname :: String, dnval :: String, dnpos :: Int }
               | Repeat Int
               | Hide Int
                deriving (Show)


data DataMap = DataMap { eid_map :: (Map.Map String [DataValue]), aid_map :: (Map.Map String [DataValue]) }

instance Show DataMap where
    show (DataMap eid_map aid_map) = "DataMap: " ++ (show eid_map) ++ "," ++ (show aid_map)

data Template = Template { xml :: Xml.Element, data_map :: DataMap }
              | NoTemplate

instance Show Template where
  show (Template xml data_map) = "Template: " ++ (show data_map)
  
  
data Templates = Templates { tmpl_map :: (Map.Map String Template) }
    deriving (Show)


templates :: IO Templates
templates = return (Templates Map.empty)


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



setElementValue tmp name value pos = do
    let dm = data_map tmp
    let em = eid_map dm
    let am = aid_map dm
    let x = xml tmp
    let elemlist = Map.findWithDefault [] name em
    let newem = Map.insert name (elemlist ++ [DataValue value pos]) em
    let newdm = DataMap newem am
    return (Template x newdm)


setAttributeValue tmp name attname value pos = do
    let dm = data_map tmp
    let em = eid_map dm
    let am = aid_map dm
    let x = xml tmp
    let attlist = Map.findWithDefault [] name am
    let newam = Map.insert name (attlist ++ [DataNameValue attname value pos]) am
    let newdm = DataMap em newam
    return (Template x newdm)



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
