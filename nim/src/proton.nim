import strutils
import strtabs
import system
import re
import tables
import typetraits

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
        id: int
        nodeType: NodeType
        parent: Node
        tag: string
        children: seq[Node]

    CData = ref object of Node
        content: string

    Element = ref object of Node
        attributes: StringTableRef
        attribute_names: seq[string]

    Document = ref object of Node
        xmldecl: string
        doctype: string
        root: Node

    Template* = ref object of RootObj
        doc: Document
        eidmap: Table[string, seq[Element]]
        aidmap: Table[string, seq[Element]]
        ridmap: Table[string, seq[Element]]

    OutputType = enum
        otFile,
        otSeq

    Output = object
        case kind: OutputType
        of otFile: f: File
        of otSeq: s: seq[string]

const
    XMLDECL_PREFIX = "<?"
    DOCTYPE_PREFIX = "<!"
    INDEX_ALL* = IndexType(pos:0, all: true)
    NULLTEMPLATES: Table[string, Template] = tables.initTable[string, Template]()

var attribnames: array[3, string]
attribnames = ["eid", "rid", "aid"]

var templates: Table[string, Template] = tables.initTable[string, Template]()

var counter: int = 0


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


proc dumpids(items:seq[Node|Element]):seq[int] =
    var s:seq[int] = @[]
    for i in items:
        add(s, i.id)
    return s


proc putmaplist[T](tab: var Table[string, seq[T]], key:string, value:T) =
    if hasKey(tab, key):
        var newval: seq[T] = tab[key] & value
        tab[key] = newval
    else:
        tab[key] = @[value]


proc idxseq(items: seq[Node], item:Node): int =
    var pos = 0
    for i in items:
        if i == item and i.id == item.id:
            return pos
        pos += 1
    return -1


proc delseq[T](items:var seq[T], item:T): int =
    var pos = idxseq(items, item)
    if pos >= 0:
        items.delete(pos)
    return pos


proc openArraySize(oa: openArray[string]): string =
    oa.len.`$`


# XML related

proc makeelem(par:Node, tagname:string, closed:bool = false): Element =
    var c: seq[Node] = @[]
    var nt = ntElement
    if closed:
        nt = ntClosedElement
    inc(counter)
    var elem = Element(id:counter, nodeType:nt, tag:tagname, children:c, parent:par)
    addtoparent(par, elem)
    return elem


proc makecdata(par:Node, cdata:string): CData =
    inc(counter)
    var elem = CData(id:counter, nodeType:ntCData, parent:par, content:cdata)
    addtoparent(par, elem)
    return elem


proc copynode(n:Node): Node =
    var nn = copy(n)
    nn.parent = n.parent
    nn


proc maketemplate(): Template =
    var em: Table[string, seq[Element]] = tables.initTable[string, seq[Element]]()
    var am: Table[string, seq[Element]] = tables.initTable[string, seq[Element]]()
    var rm: Table[string, seq[Element]] = tables.initTable[string, seq[Element]]()
    inc(counter)
    var d = Document(id:counter, nodeType:ntDocument)
    var tmp = Template(doc:d, eidmap:em, aidmap:am, ridmap:rm)
    return tmp


# xml output

proc write(o:var Output, s:string) =
    if o.kind == otFile:
        write(o.f, s)
    else:
        add(o.s, s)


proc printxmlattr(o:var Output, elem:Element) =
    for key in elem.attribute_names:
        if key notin attribnames:
            write(o, " " & key & "=\"" & elem.attributes[key] & "\"")


proc printnode(o:var Output, node:Node) =
    case node.nodeType
        of ntElement:
            write(o, "<" & node.tag)
            var elem = cast[Element](node)
            printxmlattr(o, elem)
            write(o, ">")
            for child in elem.children:
                printnode(o, child)
            write(o, "</" & node.tag & ">")
        of ntClosedElement:
            write(o, "<" & node.tag)
            var elem = cast[Element](node)
            printxmlattr(o, elem)
            write(o, " />")
        of ntDocument:
            var doc = cast[Document](node)
            if doc.xmldecl != "":
                write(o, doc.xmldecl & "\n")
            if doc.doctype != "":
                write(o, doc.doctype & "\n")
            printnode(o, doc.root)
        of ntCData:
            var cdata = cast[CData](node)
            write(o, cdata.content)
        else:
            discard


proc print*(f:File, tmp:Template) =
    var o: Output = Output(kind: otFile, f:f)
    printnode(o, tmp.doc)


proc print*(s:var seq[string], tmp:Template) =
    var o: Output = Output(kind: otSeq, s:s)
    printnode(o, tmp.doc)
    s = o.s


proc `$`(e:Node):string =
    case e.nodeType
        of ntElement:
            var elem = cast[Element](e)
            return "Element[" & elem.id.`$` & "," & elem.tag & "," & elem.attributes.`$` & "]"
        of ntClosedElement:
            var elem = cast[Element](e)
            return "ClosedElement[" & elem.id.`$` & "," & elem.tag & "," & elem.attributes.`$` & "]"
        of ntDocument:
            return ""
        of ntCData:
            var cdata = cast[CData](e)
            return "cdata[" & cdata.id.`$` & "," & replace(cdata.content, "\n", "\\n") & "]"
        else:
            discard   


# template related

proc storeattrs(tmp:Template, elem:Element) =
    if hasKey(elem.attributes, "eid"):
        putmaplist[Element](tmp.eidmap, elem.attributes["eid"], elem)
    if hasKey(elem.attributes, "aid"):
        putmaplist[Element](tmp.aidmap, elem.attributes["aid"], elem)
    if hasKey(elem.attributes, "rid"):
        putmaplist[Element](tmp.ridmap, elem.attributes["rid"], elem)


proc storeallattrs(tmp:Template, node:Node) =
    if node.nodeType == ntElement:
        var elem = cast[Element](node)
        storeattrs(tmp, elem)
    for child in node.children:
        storeallattrs(tmp, child)


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
                discard delseq(elem.parent.children, elem)
        elif idx.pos < len(elemlist):
            var elem = elemlist[idx.pos]
            discard delseq(elem.parent.children, elem)
            elemlist.delete(idx.pos)
        del(tmp.eidmap, eid)


proc replaceInternal(tmp:Template, eid:string, value:Node, idx:IndexType = INDEX_ALL):bool  =
    if hasKey(tmp.eidmap, eid):
        var elemlist = tmp.eidmap[eid]
        if idx.all:
            for elem in elemlist:
                var parent = elem.parent
                var pos = delseq(parent.children, elem)
                insert(parent.children, value, pos)
        elif idx.pos < len(elemlist):
            var elem = elemlist[idx.pos]
            var parent = elem.parent
            var pos = delseq(parent.children, elem)
            insert(parent.children, value, pos)
        return true
    return false


proc replace*(tmp:Template, eid:string, value:Template, idx:IndexType = INDEX_ALL) =
    var replacement = value.doc.root
    if replaceInternal(tmp, eid, replacement, idx):
        storeallattrs(tmp, replacement)


proc replaceHtml*(tmp:Template, eid:string, value:string, idx:IndexType = INDEX_ALL) =
    var replacement = CData(nodeType:ntCData, content:value)
    discard replaceInternal(tmp, eid, replacement, idx)


proc repeat*(tmp:Template, rid:string, count:int) =
    if hasKey(tmp.ridmap, rid):
        var elem = tmp.ridmap[rid][0]
        var parent = elem.parent
        var pos = idxseq(parent.children, elem)
        var x = 1
        while x < count:
            var newelem = copynode(elem)
            inc(counter)
            newelem.id = counter
            insert(parent.children, newelem, pos + x)
            x += 1
            storeallattrs(tmp, newelem)


proc gettemplate*(name:string, cache:bool = true, tmps:Table[string, Template] = NULLTEMPLATES): Template {.gcsafe.} =
    var local_templates = tmps
    if local_templates == NULLTEMPLATES:
        local_templates = templates
    if not hasKey(tmps, name) or not cache:
        var f = open(name)
        var s = strip(readAll(f), false, true)
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
                    var attr_names: seq[string] = @[]
                    var attrmatches: array[8, string]
                    for match in re.findAll(matches[1], re"""([^\s=]+\s*=\s*["']{1}[^"']*["']{1})"""):
                        if match =~ re"""([^=]+)=["']{1}([^"']*)""":
                            attrs[matches[0]] = matches[1]
                            attr_names.add(matches[0])
                    if strutils.startsWith(i, "</"):
                        if currentelem.tag == tagname:
                            currentelem = currentelem.parent
                    elif strutils.endsWith(i, "/>"):
                        elem = makeelem(currentelem, tagname, true)
                        elem.attributes = attrs
                        elem.attribute_names = attr_names
                    else:
                        elem = makeelem(currentelem, tagname)
                        currentelem = elem
                        elem.attributes = attrs
                        elem.attribute_names = attr_names
                    if elem != nil:
                        storeattrs(tmp, elem)
            else:
                var cdata = makecdata(currentelem, i)

        if cache:
            local_templates[name] = tmp
        else:
            return tmp
    return copy(local_templates[name])

