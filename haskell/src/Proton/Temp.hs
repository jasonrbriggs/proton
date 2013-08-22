module Proton.Temp where

import Control.Monad.State  

countStuff :: [Int] -> State Int Int
countStuff [] = do
    st <- get
    return st
countStuff (x:xs) = do
    st <- get
    let newst = st + 1
    put newst
    countStuff xs
    
testStuff :: [Int] -> State [Int] [Int]
testStuff [] = do
    st <- get
    return st
testStuff (x:xs) = do
    st <- get
    let x2 = x * 2
    let newst = st ++ [x2]
    put newst
    testStuff xs    

{-
type Stack = [Int]

pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs)

stackManip :: State Stack Int  
stackManip = do  
    push 3  
    a <- pop  
    pop
-}


{-
data Foo a b = Foo a (b -> Foo a b)
             | Nil

noOp (0, 0) = Nil
noOp (x1, x2) = do
    Foo (x1, x2) noOp

getX1 (Foo a b) = do
    let (x1, x2) = a
    x1
getX2 (Foo a b) = do
    let (x1, x2) = a
    x2

getFn (Foo a b) = b
-}

{-
data Foo a b = Foo a (b -> Foo a b)

noOp (x1, x2) = do
    Foo (x1, x2) noOp

getX1 (Foo a b) = do
    let (x1, x2) = a
    x1
getX2 (Foo a b) = do
    let (x1, x2) = a
    x2

getFn (Foo a b) = b
-}