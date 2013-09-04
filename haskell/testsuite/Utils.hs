module Utils where
    
import Test.HUnit
    
stripWhitespace "" = ""
stripWhitespace (s:ss) = do
    if s == ' ' || s == '\r' || s == '\t' || s == '\n'
        then stripWhitespace ss
        else [s] ++ stripWhitespace ss

checkResult testOutput resultFile = do
    chk <- readFile resultFile
    let chkOutput = stripWhitespace testOutput
    let chkResult = stripWhitespace chk
    assertEqual ("Output does not match result file" ++ resultFile) chkOutput chkResult