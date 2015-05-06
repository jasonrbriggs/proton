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

module Text.Proton.XmlTypes where

    
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