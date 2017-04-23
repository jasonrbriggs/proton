import strutils
import strtabs
import system
import re
import tables

type
    NodeType = enum
        ntProcessingInstruction,
        ntCData,
        ntComment,
        ntElement,
        ntClosedElement,
        ntDocument

    IndexType = object
        pos: int
        all: bool

    Attribute = object
        name*: string
        value*: string

    Node = ref object of RootObj
        nodeType: NodeType
        parent: Node
        tag: string
        children: seq[Node]

    CData = ref object of Node
        content: string

    Element = ref object of Node
        attributes: StringTableRef

    Document = ref object of Node
        xmldecl: string
        doctype: string
        root: Node

    Template* = ref object of RootObj
        doc: Document
        eidmap: Table[string, seq[Element]]
        aidmap: Table[string, seq[Element]]
        ridmap: Table[string, seq[Element]]


const
    XMLDECL_PREFIX = "<?"
    DOCTYPE_PREFIX = "<!"
    INDEX_ALL* = IndexType(pos:0, all: true)


var attribnames: array[3, string]
attribnames = ["eid", "rid", "aid"]

var templates: Table[string, Template] = tables.initTable[string, Template]()


# General utilities


proc indexof*(i:int): IndexType =
    return IndexType(pos: i)


proc addtoparent(par:Node, elem:Node) =
    if par of Document:
        var parent = cast[Document](par)
        parent.root = elem
    else:
        var tagelem = cast[Element](par)
        tagelem.children.add(elem)


proc copy[T](x: T): T {.inline.} =
    var tmp: T
    deepCopy(tmp, x)
    tmp


proc putmaplist[T](tab: var Table[string, seq[T]], key:string, value:T) =
    if hasKey(tab, key):
        var newval: seq[T] = tab[key] & value
        tab[key] = newval
    else:
        tab[key] = @[value]


proc delseq[T](items:var seq[T], item:T) =
    var pos = 0
    for i in items:
        if i == item:
            items.delete(pos)
            return
        pos += 1
    return


proc openArraySize(oa: openArray[string]): string =
    oa.len.`$`



# XML related

proc makeelem(par:Node, tagname:string, closed:bool = false): Element =
    var c: seq[Node] = @[]
    var nt = ntElement
    if closed:
        nt = ntClosedElement
    var elem = Element(nodeType:nt, tag:tagname, children:c, parent:par)
    addtoparent(par, elem)
    return elem


proc makecdata(par:Node, cdata:string): CData =
    var elem = CData(nodeType: ntCData, parent:par, content:cdata)
    addtoparent(par, elem)
    return elem


proc maketemplate(): Template =
    var em: Table[string, seq[Element]] = tables.initTable[string, seq[Element]]()
    var am: Table[string, seq[Element]] = tables.initTable[string, seq[Element]]()
    var rm: Table[string, seq[Element]] = tables.initTable[string, seq[Element]]()
    var d = Document(nodeType:ntDocument)
    var tmp = Template(doc:d, eidmap:em, aidmap:am, ridmap:rm)
    return tmp


# xml output

proc printxmlattr(f:File, elem:Element) =
    for kp in strtabs.pairs(elem.attributes):
        if kp.key notin attribnames:
            write(f, " " & kp.key & "=\"" & kp.value & "\"")

proc printnode(f:File, node:Node) =
    case node.nodeType
        of ntElement:
            write(f, "<" & node.tag)
            var elem = cast[Element](node)
            printxmlattr(f, elem)
            write(f, ">")
            for child in elem.children:
                printnode(f, child)
            write(f, "</" & node.tag & ">")
        of ntClosedElement:
            write(f, "<" & node.tag)
            var elem = cast[Element](node)
            printxmlattr(f, elem)
            write(f, " />")
        of ntDocument:
            var doc = cast[Document](node)
            write(f, doc.xmldecl & "\n")
            write(f, doc.doctype & "\n")
            printnode(f, doc.root)
        of ntCData:
            var cdata = cast[CData](node)
            write(f, cdata.content)
        else:
            discard


proc print*(f:File, tmp:Template) =
    printnode(f, tmp.doc)


# template related


proc setvalue*(tmp:Template, eid:string, value:string, idx:IndexType = INDEX_ALL, append:bool = false) =
    if hasKey(tmp.eidmap, eid):
        var elemlist = tmp.eidmap[eid]
        if idx.all:
            for elem in elemlist:
                if not append:
                    elem.children = @[]
                var cdata = makecdata(elem, value)
        elif idx.pos < len(elemlist):
            var elem = elemlist[idx.pos]
            if not append:
                elem.children = @[]
            var cdata = makecdata(elem, value)


proc setattribute*(tmp:Template, aid:string, name:string, value:string, idx:IndexType = INDEX_ALL) =
    if hasKey(tmp.aidmap, aid):
        var elemlist = tmp.aidmap[aid]
        if idx.all:
            for elem in elemlist:
                elem.attributes[name] = value
        elif idx.pos < len(elemlist):
            var elem = elemlist[idx.pos]
            elem.attributes[name] = value


proc hide*(tmp:Template, eid:string, idx:IndexType = INDEX_ALL) =
    if hasKey(tmp.eidmap, eid):
        var elemlist = tmp.eidmap[eid]
        if idx.all:
            for elem in elemlist:
                delseq(elem.parent.children, elem)
        elif idx.pos < len(elemlist):
            var elem = elemlist[idx.pos]
            delseq(elem.parent.children, elem)
            elemlist.delete(idx.pos)
        del(tmp.eidmap, eid)


proc gettemplate*(name:string): Template =
    if not hasKey(templates, name):
        var f = open(name)
        var s = readAll(f)
        close(f)

        var tmp = maketemplate()
        var currentelem: Node = tmp.doc
        
        for i in re.findAll(s, re"<[^>]+>|[^<]+"):
            if strutils.startsWith(i, XMLDECL_PREFIX):
                tmp.doc.xmldecl = i
            elif strutils.startsWith(i, DOCTYPE_PREFIX):
                tmp.doc.doctype = i
            elif strutils.startsWith(i, '<'):
                var tag = re.findAll(i, re"<([^\s>]+)")
                if i =~ re"</?([^ >]+)([^>]*)":
                    var elem: Element
                    var tagname = matches[0]
                    var attrs = newStringTable()
                    var attrmatches: array[8, string]
                    for match in re.findAll(matches[1], re"""([^\s=]+\s*=\s*["']{1}[^"']*["']{1})"""):
                        if match =~ re"""([^=]+)=["']{1}([^"']*)""":
                            attrs[matches[0]] = matches[1]
                    if strutils.startsWith(i, "</"):
                        if currentelem.tag == tagname:
                            currentelem = currentelem.parent
                    elif strutils.endsWith(i, "/>"):
                        elem = makeelem(currentelem, tagname, true)
                        elem.attributes = attrs
                    else:
                        elem = makeelem(currentelem, tagname)
                        currentelem = elem
                        elem.attributes = attrs
                    if elem != nil:
                        if hasKey(elem.attributes, "eid"):
                            putmaplist[Element](tmp.eidmap, elem.attributes["eid"], elem)
                        if hasKey(elem.attributes, "aid"):
                            putmaplist[Element](tmp.aidmap, elem.attributes["aid"], elem)
            else:
                var cdata = makecdata(currentelem, i)

        templates[name] = tmp
    return copy(templates[name])    

