import strutils
import unittest

import proton

proc stripWhitespace(content:string): string =
    var lines: seq[string] = @[]
    for line in splitLines(content):
        lines.add(strip(line, true, true))
    return join(lines)


proc compare(c1:string, c2:string) =
    var fc1 = open(c1)
    var sc1 = stripWhitespace(readAll(fc1))
    close(fc1)
    
    var fc2 = open(c2)
    var sc2 = stripWhitespace(readAll(fc2))
    close(fc2)

    assert(sc1 == sc2, "" & c1 & " is not equal to expected " & c2)
    echo "" & c2 & " is okay"


proc writeandcompare(tmp:Template, fname:string, compareto:string) =
    var f = open(fname, fmWrite)
    print(f, tmp)
    close(f)
    compare(compareto, fname)


suite "Proton tests":
    setup:
        echo "\n"
  
    test "basic functionality":
        var tmp = gettemplate("../resources/basic.xhtml")
        setvalue(tmp, "title", "Basic Xhtml Page")
        setvalue(tmp, "content", "Content goes here")
        setvalue(tmp, "link", "Link goes here")
        setattribute(tmp, "link", "href", "http://www.duckduckgo.com")
        
        var f = open("tmp/basic1.xhtml", fmWrite)
        print(f, tmp)
        close(f)
        compare("../resources/basic-result.xhtml", "tmp/basic1.xhtml")

        var tmp2 = gettemplate("../resources/basic.xhtml")

        var f2 = open("tmp/basic2.xhtml", fmWrite)
        print(f2, tmp2)
        close(f2)
        compare("../resources/basic-unprocessed-result.xhtml", "tmp/basic2.xhtml")


    test "basic - append":
        var tmp = gettemplate("../resources/basic-append.xhtml")

        setvalue(tmp, "title", "Append Title")
        setvalue(tmp, "content", "Append Content")

        setvalue(tmp, "head", """<meta name="description" content="append description" />""", INDEX_ALL, true)
        setvalue(tmp, "content", """<p>some additional content</p>""", INDEX_ALL, true)

        var f = open("tmp/basic-append.xhtml", fmWrite)
        print(f, tmp)
        close(f)
        compare("../resources/basic-append-result.xhtml", "tmp/basic-append.xhtml")

    test "hiding 1":
        var tmp = gettemplate("../resources/hiding.xhtml")
        setvalue(tmp, "title", "Hiding Xhtml Page")
        hide(tmp, "hidden-element")

        writeandcompare(tmp, "tmp/hiding.xhtml", "../resources/hiding-result.xhtml")

