module RunTests where

import Test.HUnit

import TestXml
import TestXmlInternal
import TestTemplateBasic
import TestTemplateRepeat
import TestTemplateHiding
import TestTemplateBlogExample

main = do
    runTestTT TestXmlInternal.xml_tests
    runTestTT TestXml.xml_tests
    runTestTT TestTemplateBasic.template_tests
    runTestTT TestTemplateRepeat.template_tests
    runTestTT TestTemplateHiding.template_tests
    runTestTT TestTemplateBlogExample.template_tests