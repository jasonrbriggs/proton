module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import TestXml
import TestXmlInternal
import TestTemplateBasic
import TestTemplateRepeat
import TestTemplateHiding
import TestTemplateBlogExample

{-
main = do
    runTestTT TestTemplateBasic.template_tests
    runTestTT TestTemplateRepeat.template_tests
    runTestTT TestTemplateHiding.template_tests
    runTestTT TestTemplateBlogExample.template_tests
-}
  
tests = hUnitTestToTests $ TestList (TestXml.xmlTests ++
                                     TestXmlInternal.xmlTests ++
                                     TestTemplateBasic.templateTests ++
                                     TestTemplateRepeat.templateTests ++
                                     TestTemplateHiding.templateTests ++
                                     TestTemplateBlogExample.templateTests
                                     )

main = defaultMain tests