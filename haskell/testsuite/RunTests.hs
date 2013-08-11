module RunTests where

import Test.HUnit

import TestXml
import TestTemplate

main = do
    runTestTT TestXml.tests