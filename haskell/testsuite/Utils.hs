module Utils where
    
stripWhitespace "" = ""
stripWhitespace (s:ss) = do
    if s == ' ' || s == '\r' || s == '\t' || s == '\n'
        then stripWhitespace ss
        else [s] ++ stripWhitespace ss