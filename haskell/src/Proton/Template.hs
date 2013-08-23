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


renderReplace dm (s, atts, xs) = do
    let newxs = if containsAttribute "eid" atts then renderReplaceEID dm xs (findAttribute "eid" atts) else xs
    -- todo
    let newatts = atts
    
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
