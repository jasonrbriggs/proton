module Proton.Template where

import qualified Data.Map as Map
import Data.Maybe as Mb
import Control.Monad.State

import Proton.Xml as Xml


data DataValue = DataValue { dval :: String, dpos :: Int, dcurrent_pos :: Int }
               | DataNameValue { dnname :: String, dnval :: String, dnpos :: Int, dncurrent_pos :: Int }
               | Repeat Int
               | Hide Int
                deriving (Show)


data DataMap = DataMap { eid_map :: (Map.Map String DataValue), aid_map :: (Map.Map String DataValue) }

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


--load :: (Monad m) => Templates -> String -> IO Templates
loadTemplate tmps name = do
    if (Map.member name (tmpl_map tmps)) then return tmps
    else do
        x <- parseXmlFile name
        let t = Template x (DataMap Map.empty Map.empty)
        let templateMap = tmpl_map tmps
        return tmps { tmpl_map = Map.insert name t templateMap }


--get           :: Templates -> String -> Template
getTemplate tmps name = do
    let mp = tmpl_map tmps
    return (Map.findWithDefault NoTemplate name mp)


setElementValue tmp name value pos = do
    let dm = data_map tmp
    let em = eid_map dm
    let am = aid_map dm
    let x = xml tmp
    let newem = Map.insert name (DataValue value pos 0) em
    let newdm = DataMap newem am
    return (Template x newdm)


renderReplace dm (s, atts, xs) = do
    let em = eid_map dm
    let eid = findAttributeValue "eid" atts
    case eid of
        Just e -> do
            let dv = Map.lookup e em
            case dv of
                Just v -> do
                    let currentpos = dcurrent_pos v
                    if currentpos == (dpos v)
                        then RenderCallbackFn (s, atts, [Element Raw (dval v) [] []]) (renderReplace dm)
                        else do
                            --let v2 = v{dcurrent_pos = (currentpos + 1)}
                            --let em = Map.alter e v2 em
                            --RenderCallbackFn (s, atts, [Element Raw (dval v) [] []]) (renderReplace em)
                            RenderCallbackFn (s, atts, [Element Raw (dval v) [] []]) (renderReplace dm)
                _ -> RenderCallbackFn (s, atts, xs) (renderReplace dm)
        Nothing -> RenderCallbackFn (s, atts, xs) (renderReplace dm)


--renderTemplate :: Template -> String
renderTemplate tmp = do
    let dm = data_map tmp
    let x = xml tmp
    let renderReplaceInternal = renderReplace dm
    let s = render' x renderReplaceInternal
    return s

{-
setElementValue name value pos tmp = do
    let dm = data_map tmp
    let x = xml tmp
    let children = getChildren x

    let (children, dm) = runState (setElementValue' name value pos 0 children) dm
    Element Root "" [] children
-}

{-
setElementValue' :: String -> String -> Integer -> Integer -> [Element] -> State DataMap [Element]
setElementValue' name value pos currentpos [] = return []
setElementValue' name value pos currentpos (x:xs) = do
    dm <- get
    let atts = getAttributes x
    let eid = findAttributeValue "eid" atts
    case eid of
        Just e ->
            if e == name then do
                if pos == currentpos then return $ [x] ++ xs
                else return $ [x] ++ xs
            else return $ [x] ++ xs
        _      -> return $ [x] ++ xs


setElementValueDefault :: String -> String -> Integer -> Integer -> Element -> [Element] -> State DataMap [Element]
setElementValueDefault name value pos currentpos x xs = do
    let children = fst (setElementValue' name value pos currentpos xs)
    return ([x] ++ children)
-}        
{-
        Just e ->
            if e == name then do
                if pos == currentpos then return $ [x] ++ xs
                else return $ [x] ++ xs
            else setElementValueDefault name value pos currentpos x xs
        _      -> setElementValueDefault name value pos currentpos x xs
-}
{-
setElementValueDefault name value pos currentpos x xs = do
    let (children, dm) = setElementValue' name value pos currentpos xs
    return [x] ++ children
-}

{-
setElementValue tmp name value pos = do
    let dm = data_map tmp
    let em = eid_map dm
    let am = aid_map dm
    let x = xml tmp
    let newem = Map.insert name (DataValue value pos 0) em
    let newdm = DataMap newem am
    return (Template x newdm)


renderReplace dm (s, atts, xs) = do
    let em = eid_map dm
    let eid = findAttributeValue "eid" atts
    case eid of
        Just e -> do
            let dv = Map.lookup e em
            case dv of
                Just v -> do
                    let currentpos = dcurrent_pos v
                    if currentpos == (dpos v)
                        then RenderCallbackFn (s, atts, [Element Raw (dval v) [] []]) (renderReplace dm)
                        else do
                            --let v2 = v{dcurrent_pos = (currentpos + 1)}
                            --let em = Map.alter e v2 em
                            --RenderCallbackFn (s, atts, [Element Raw (dval v) [] []]) (renderReplace em)
                            RenderCallbackFn (s, atts, [Element Raw (dval v) [] []]) (renderReplace dm)
                _ -> RenderCallbackFn (s, atts, xs) (renderReplace dm)
        Nothing -> RenderCallbackFn (s, atts, xs) (renderReplace dm)
-}


{-
setElementValue name value pos tmp = do
    let x = xml tmp
    let children = getChildren x

    Root [setElementValue' name value pos 0 children]

setElementValue' name value pos currentpos (x:xs) =
    let atts = getAttributes x
    let eid = findAttributesValue "eid" atts
    case eid of
        Just e ->
            if e == name then do
                if pos == currentpos then
                    
                else setElementValueDefault name value pos currentpos (x:xs)
                
            else setElementValueDefault name value pos currentpos (x:xs)
        _      -> setElementValueDefault name value pos currentpos (x:xs)
            
setElementValueDefault name value pos currentpos (x:xs) =
    [x] ++ (setElementValue' name value pos currentpos xs)
-}
{-
setElementValue' name value pos currentpos xml = do
    let atts = getAttributes xml
    let eid = findAttributesValue "eid" atts
    case eid of
        Just e -> 
-}
{-
repeatElement :: Template -> String -> Int -> Template
repeatElement tmp name count = do
    tmp
-}      

{-    
--setElementValue :: Template -> String -> String -> Template
setElementValue tmp name value pos = do
    let em = eid_map tmp
    let am = aid_map tmp
    let x = xml tmp
    let newem = Map.insert name (DataValue value pos 0) em
    return (Template x newem am)


renderReplace em (s, atts, xs) = do
    let eid = findAttributeValue "eid" atts
    case eid of
        Just e -> do
            let dv = Map.lookup e em
            case dv of
                Just v -> do
                    let currentpos = dcurrent_pos v
                    if currentpos == (dpos v)
                        then RenderCallbackFn (s, atts, [RawElement (dval v)]) (renderReplace em)
                        else do
                            let v2 = v{dcurrent_pos = (currentpos + 1)}
                            let em = Map.alter e v2 em
                            RenderCallbackFn (s, atts, [RawElement (dval v)]) (renderReplace em)
                _ -> RenderCallbackFn (s, atts, xs) (renderReplace em)
        Nothing -> RenderCallbackFn (s, atts, xs) (renderReplace em)


--renderTemplate :: Template -> String
renderTemplate tmp = do
    let em = eid_map tmp
    let x = xml tmp
    let renderReplaceInternal = renderReplace em
    let s = render' x renderReplaceInternal
    return s
  
--renderWithReplacement :: (Map String String) -> String -> [Attribute] -> [Element] -> (String, [Attribute], [Element])
--renderWithReplacement dm s as xs = (s, as, xs)



-}