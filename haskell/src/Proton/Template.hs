module Proton.Template where

import qualified Data.Map as Map
import Data.Maybe as Mb

import Proton.Xml as Xml

data DataValue = DataValue String Int
               | DataNameValue String String Int
               | Repeat Int
               | Hide Int
                deriving (Show)

data Template = Template { xml :: IO Xml.Element, eid_map :: (Map.Map String DataValue), aid_map :: (Map.Map String DataValue) }
              | NoTemplate

instance Show Template where
    show (Template xml eid_map aid_map) = "Template: " ++ (show eid_map) ++ "," ++ (show aid_map)

data Templates = Templates { tmpl_map :: (Map.Map String Template) }
    deriving (Show)

templates :: Templates
templates = Templates Map.empty


load :: Templates -> String -> Templates
load tmps name = do
    if (Map.member name (tmpl_map tmps)) then tmps
    else do
        let x = parseXmlFile name
        let t = Template x Map.empty Map.empty
        let templateMap = tmpl_map tmps
        tmps { tmpl_map = Map.insert name t templateMap }


get           :: Templates -> String -> Template
get tmps name = do
    let mp = tmpl_map tmps
    Map.findWithDefault NoTemplate name mp


repeatElement :: Template -> String -> Int -> Template
repeatElement tmp name count = do
    tmp
    
setElementValue :: Template -> String -> String -> Template
setElementValue tmp name value = do
    let em = eid_map tmp
    let am = aid_map tmp
    let x = xml tmp
    let newem = Map.insert name (DataValue value 0) em
    Template x newem am


renderTemplate :: Template -> String
renderTemplate tmp = do
    let dm = eid_map tmp
    let x = xml tmp
    --render x
    ""
    
{-
renderWithReplacement :: (Map String String) -> String -> [Attribute] -> [Element] -> (String, [Attribute], [Element])
renderWithReplacement dm s as xs = (s, as, xs)
-}


