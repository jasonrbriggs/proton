module RunTests where

import Test.HUnit

import TestXml
import TestTemplate

main = do
    runTestTT TestXml.xml_tests
    runTestTT TestTemplate.template_tests