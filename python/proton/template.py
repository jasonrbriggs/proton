# -*- coding: utf-8 -*-
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


import copy
import os
import re
import sys
import types    

try:
    from io import StringIO
except ImportError:
    from StringIO import StringIO

from proton import xmlutils, utils

try:
    from lxml import etree
except ImportError:
    print("WARNING: lxml is not available, performance will be affected")
    from xml.etree import ElementTree as etree
    etree._ElementInterface.getparent = xmlutils.getparent
    __fromstring = etree.fromstring
    
    def fromstring(s):
        et = __fromstring(s)
        xmlutils.loadparents(et, None)
        return et
    etree.fromstring = fromstring


#
# tuple of sequence types
#
SEQ_TYPES = (list, tuple)

#
# extension to mime-type map 
#
EXT_MAP = {
    'xml' : 'text/xml; charset=utf-8',
    'html' : 'text/html; charset=utf-8',
    'xhtml' : 'text/html; charset=utf-8'
}

#
# the contents of these elements should be translated
#
TRANSLATED_ELEMENTS = ('h1', 'h2', 'h3', 'h4', 'h5', 'p', 'span', 'th', 'td', 'button', 'title')

#
# these elements must have body content (stop etree from removing the close tag and screwing
# up rendering)
#
BODY_REQUIRED_ELEMENTS = ('textarea', 'iframe', 'br', 'a', 'p', 'div', 'td', 'th')

#
# regex to find XML elements in content
#
ELEM_RE = re.compile(r'''<[^\s]*\s*(?:[^=\s]*\s*=\s*"|'[^"']*"|'\s*)*\/?>''')

#
# when the os environ variable 'url_prefix' is set, we use URL_RE to find references to
# url(....) in the template (so we can automagically prefix urls in style sheet includes, etc
#
URL_RE = re.compile(r'''(url\(("|')?)/''')
if 'url_prefix' in os.environ:
    urlprefix = os.environ['url_prefix']
else:
    urlprefix = None


DOCTYPE_RE = re.compile(r'(<!DOCTYPE[^>]*>)')


class Append(object):
    def __init__(self, xml):
        self.elem = etree.fromstring(('<root>%s</root>' % xml).encode())
    
    def __call__(self, elem):
        for child in self.elem.getchildren():
            elem.append(child)


class Function(object):
    '''
    Base class for functions.
    '''
    def __call__(self, text):
        return ''


class Repeater(Function):
    def __init__(self, count, childrenonly=False):
        self.count = count
        self.childrenonly = childrenonly
        
    def __call__(self):
        return self.count
        
    def __str__(self):
        return str(self.count)

        
class Replace(Function):
    '''
    A function for string tokenisation of some text with arguments.
    '''
    def __init__(self, arguments, translate=None):
        self.arguments = arguments
        self.translate = translate
        
    def __call__(self, text):
        if self.translate:
            tr = self.translate(text)
        else:
            tr = text
        if self.arguments:
            return tr % self.arguments
        else:
            return tr


class Hide(Function): 
    '''
    Empty function for hiding elements
    '''
    pass


class EternalIterator(object):
    '''
    An iterator that returns the same value over and over again
    '''
    def __init__(self, value):
        self.value = value
        
    def pop(self):
        return self.value

class ValueMap(object):
    def __init__(self):
        self.valmap = { }
        self.iterators = { }
        
    def setvalue(self, key, value, index = 0):
        if index == '*':
            self.valmap[key] = EternalIterator(value)
        else:
            if key not in self.valmap:
                self.valmap[key] = [ ]
            if index >= len(self.valmap[key]):
                if not isinstance(value, SEQ_TYPES):
                    value = [ value ]
                
                for x in range(len(self.valmap[key]), index):
                    self.valmap[key].append(None)
                
                self.valmap[key] = value + self.valmap[key]
            else:
                self.valmap[key][index] = value
    
    def __getitem__(self, key):
        if key not in self.iterators:
            if key in self.valmap:
                self.iterators[key] = self.valmap[key]
            else:
                return None
        try:
            return self.iterators[key].pop()
        except:
            return None
            
    def __contains__(self, key):
        return key in self.valmap
            
    def peek(self, key):
        rtn = self[key]
        self.iterators[key].append(rtn)
        return rtn
        
    def keys(self):
        return self.valmap.keys()


class AttributeMap(object):
    def __init__(self):
        self.attmap = { }
        
    def __getitem__(self, key):
        return self.attmap[key]
        
    def setattribute(self, key, attname, attval, index = 0):
        if key not in self.attmap:
            self.attmap[key] = ValueMap()
        self.attmap[key].setvalue(attname, attval, index)

    def __contains__(self, key):
        return key in self.attmap

        
class Templates():
    def __init__(self, template_path=None):
        self.template_path = template_path
        
    def __getitem__(self, name):
        tmp = Template(self.template_path, name)
        tmp.mimetype = EXT_MAP[name[name.rfind('.') + 1:]]
        return tmp


def copy_elem(elem):
    newelem = copy.copy(elem)
    newelem.attrib = copy.copy(elem.attrib)
    return newelem


class Template(object):
    entities = { 'nbsp' : chr(160), 'copy' : chr(169) }
    elementtrees = { }
    class_cache = { }
    
    def __init__(self, template_path, filename):
        self.template_path = template_path
        self.et = self.__loadtree(filename)
        self.rid_map = ValueMap()
        self.template_map = { }
        self.eid_map = ValueMap()
        self.aid_map = AttributeMap()
        self.translate = None           # translate function
        self.buffer = None
        self.doctype = ''
        self.translated_elements = TRANSLATED_ELEMENTS

    def __loadtree(self, filename):
        """
        Load an xml file parsing into an elementtree structure.
        """
        if self.template_path:
            path = os.path.join(self.template_path, filename)
        else:
            path = filename
            
        if path not in Template.elementtrees:
            #parser = ElementTree.XMLTreeBuilder()
            #parser.entity.update(Template.entities)
            xml = open(path).read()
            
            # find the doctype of the template 
            mat = DOCTYPE_RE.search(xml)
            if mat:
                self.doctype = '%s\n' % mat.group(1)

            et = etree.fromstring(xml.encode())
            Template.elementtrees[path] = et
            return et
        else:
            return copy.deepcopy(Template.elementtrees[path])    

    def append(self, eid, xml, index = 0):
        self.setelement(eid, Append(xml), index)
        
    def hide(self, eid, index = 0):
        self.setelement(eid, Hide(), index)
        
    def include(self, eid, filename):
        """
        Include a template file at the specified eid location
        
        @param eid: the eid of the element which should have its content replaced by the template
        @param filename: the filename of the template
        """
        self.template_map[eid] = self.__loadtree(filename)
        
    def repeat(self, rid, count, index = 0, childrenonly = False):
        """
        Repeat either an element, or its children a number of times
        
        @param rid: the rid of the element to repeat
        @param index: the index to repeat
        @param childrenonly: repeat only the children or the element itself
        """
        self.rid_map.setvalue(rid, Repeater(count, childrenonly), index)
        
    def replaceattribute(self, aid, attname, arguments, index = 0):
        self.setattribute(aid, attname, Replace(arguments, self.translate), index)
        
    def replaceelement(self, eid, arguments, index = 0):
        self.setelement(eid, Replace(arguments, self.translate), index)
        
    def setattribute(self, aid, attname, value, index = 0):
        self.aid_map.setattribute(aid, attname, value, index)
        
    def setelement(self, eid, value, index = 0):
        if type(value) in SEQ_TYPES:
            self.repeat(eid, len(value))
            for x in range(0, len(value)):
                self.setelement(eid, value[x], index + x)
            return
        if self.translate:
            value = self.translate(value)
        self.eid_map.setvalue(eid, value, index)
        self.__setproperties(eid, value, index)
        
    def __setproperties(self, eid, value, index = 0):
        if value.__class__ not in Template.class_cache:
            props = [ ]
            for name in dir(value.__class__):
                prop = getattr(value.__class__, name)
                if type(prop) == property and hasattr(prop, 'fget'):
                    props.append((name, prop))
            Template.class_cache[value.__class__] = props
        for (name, prop) in Template.class_cache[value.__class__]:
            elemname = ''.join([eid, ':', name])
            self.setelement(elemname, prop.fget(value), index)
            self.setattribute(eid, name, prop.fget(value), index)
        
    def __iterate(self, elem, write = True):
        if 'rid' in elem.attrib:
            rid = elem.attrib.pop('rid')
            if rid in self.rid_map:
                counter = self.rid_map[rid]
                if counter:
                    cmax = counter() - 1
                    if not counter.childrenonly:
                        idx = xmlutils.index(elem) + 1
                        newelems = []
                        for c in range(0, cmax):
                            newelems.append(copy.deepcopy(elem))
                        for e in newelems:
                            elem.getparent().insert(idx, e)
                        self.__iterate(elem.getparent(), False)
                    else:
                        elem2 = copy.deepcopy(elem)
                        for c in range(0, cmax):
                            for child in elem2.getchildren():
                                elem.append(child)
                            if c < cmax:
                                elem2 = copy.deepcopy(elem2)
                        self.__processchildren(elem)
                    return
            # special case -- with repeated elements, either you want to repeat them
            # or get rid of them...
            elif rid in self.eid_map and isinstance(self.eid_map.peek(rid), Hide):
                return
                
        if self.translate and elem.text and utils.clean(elem.text) != '' \
                and elem.tag in self.translated_elements:
            elem.text = self.translate(elem.text)
                    
        if 'eid' in elem.attrib:
            eid = elem.attrib.pop('eid')
            if eid in self.template_map:
                et = self.template_map[eid]
                xmlutils.replaceelement(elem, et)
                self.__iterate(et)
                return
            else:
                try:
                    val = self.eid_map[eid]
                    if not val:
                        pass
                    elif isinstance(val, Hide):
                        return
                    elif isinstance(val, Append):
                        val(elem)
                    elif isinstance(val, Function):
                        elem.text = val(elem.text)
                    else:
                        elem.text = val
                    if ELEM_RE.search(elem.text) >= 0:
                        xmlutils.parseelement(elem) 
                    elem.text = elem.text.replace('&lt;', '<').replace('&gt;', '>')
                except:
                    pass
            
        if 'aid' in elem.attrib:
            aid = elem.attrib.pop('aid')
            if aid in self.aid_map:
                for attrname in self.aid_map[aid].keys():
                    try:
                        val = self.aid_map[aid][attrname]
                        if not attrname in elem.attrib:
                            continue
                        if not val or val == '':
                            if attrname in elem.attrib:
                                elem.attrib.pop(attrname)
                        elif isinstance(val, Function):
                            elem.attrib[attrname] = val(elem.attrib[attrname])
                        else:
                            elem.attrib[attrname] = val
                    except RuntimeError:
                        _, e, _ = sys.exc_info()
                        print(e)
                        pass
        
        #
        # fix for element tree not closing certain elements properly
        #
        if elem.tag in BODY_REQUIRED_ELEMENTS and elem.text is None:
            elem.text = '\n'
            
        #
        # if a url prefix is specified, use this in certain elements which relatively
        # reference a url
        #
        if urlprefix:
            if 'href' in elem.attrib and elem.attrib['href'].startswith('/'):
                elem.attrib['href'] = utils.join(urlprefix, elem.attrib['href'])
            if 'src' in elem.attrib and elem.attrib['src'].startswith('/'):
                elem.attrib['src'] = utils.join(urlprefix, elem.attrib['src'])
            if 'action' in elem.attrib and elem.attrib['action'].startswith('/'):
                elem.attrib['action'] = utils.join(urlprefix, elem.attrib['action'])
            if elem.tag == 'style' and URL_RE.search(elem.text):
                elem.text = URL_RE.sub('\g<1>%s/' % urlprefix, elem.text)
              
        if write:
            self.buffer.write('<%s' % elem.tag)
            for attrib in elem.attrib.keys():
                self.buffer.write(' %s="%s"' % (attrib, elem.attrib[attrib]))
            self.buffer.write('>')
            if elem.text is not None and elem.text != '':
                self.buffer.write(elem.text)
                
        self.__processchildren(elem)
        
        if write:
            self.buffer.write('</%s>' % elem.tag)

                    
    def __processchildren(self, elem):
        #
        # loop through the children of this element
        #
        for child in elem.getchildren()[:]:
            self.__iterate(child)

    def __str__(self):
        self.buffer = StringIO()
        self.__iterate(self.et)
        s = self.buffer.getvalue()
        self.buffer.close()
        return self.doctype + s
