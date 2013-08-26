module RunTests where

import Test.HUnit

import TestXml
import TestTemplateBasic
import TestTemplateRepeat
import TestTemplateHiding
import TestTemplateBlogExample

main = do
    runTestTT TestXml.xml_tests
    runTestTT TestTemplateBasic.template_tests
    runTestTT TestTemplateRepeat.template_tests
    runTestTT TestTemplateHiding.template_tests
    runTestTT TestTemplateBlogExample.template_tests