module Proton.XmlTypes where

    
data Attribute = Attribute { attname :: String, attvalue :: String, occ :: Integer }
               | NoAttribute
                 deriving (Show, Eq)


data ElementType = Root
                 | Raw
                 | Open
                 | Closed
                 deriving (Show, Eq)


data Element = Element ElementType String [Attribute] [Element]
               deriving (Show, Eq)


data RenderCallbackFn a b = RenderCallbackFn a (b -> RenderCallbackFn a b)