import io
import os
import re
import copy

elem_re = re.compile(r'(<\?[^>]+\?>|<![^>]+>|<\/|\/>|<|>|[^<>\s]+|\s+)', re.DOTALL | re.MULTILINE)
attrib_re = re.compile(r'\s*([^=]+)="([^"]*)"')
SEQ_TYPES = (list, tuple)


class Element(object):

    def __init__(self, tag=None, parent=None):
        self.tag = tag
        self.parent = parent
        self.children = []
        self.attribs = {}
        self.attrib_keys = []
        self.closed = False

    def __deepcopy__(self, memo):
        result = self.__class__()
        result.tag = self.tag
        result.parent = self.parent
        result.children = copy.deepcopy(self.children, memo)
        for child in result.children:
            child.parent = result
        result.attribs = copy.deepcopy(self.attribs, memo)
        result.attrib_keys = copy.deepcopy(self.attrib_keys, memo)
        result.closed = self.closed
        return result

    def __str__(self):
        l = ['%s:%s[' % (self.tag, hash(self))]
        for key in self.attrib_keys:
            l.append('%s=%s,' % (key, self.attribs[key]))
        l.append(']')
        return ''.join(l)

    def parse(self, src, template):
        contentiter = elem_re.finditer(src)
        in_tag = False

        current_elem = self

        tag_content = ''

        try:
            while 1:
                text = next(contentiter).group(1)
                #print('>>> here %s' % text)

                if text.startswith('<?'):
                    current_elem.children.append(TextElement(text))

                elif text.startswith('<!'):
                    current_elem.children.append(TextElement(text))

                elif text == '</':
                    template.check_element(current_elem)
                    next(contentiter).group(1)

                    current_elem = current_elem.parent

                elif text == '/>':
                    self.__parse_tag(current_elem, tag_content)
                    in_tag = False
                    tag_content = ''
                    template.check_element(current_elem)
                    current_elem.closed = True
                    current_elem = current_elem.parent

                elif text == '<':
                    in_tag = True
                    tag = next(contentiter).group(1)
                    elem = Element(tag, current_elem)
                    current_elem.children.append(elem)
                    current_elem = elem

                elif text == '>':
                    self.__parse_tag(current_elem, tag_content)
                    in_tag = False
                    tag_content = ''

                else:
                    if in_tag:
                        tag_content += text
                    else:
                        # append the text to the prior TextElement if present (otherwise add new)
                        if len(current_elem.children) > 0 and type(current_elem.children[-1]) == TextElement:
                            current_elem.children[-1].content += text
                        else:
                            te = TextElement(text)
                            current_elem.children.append(te)

        except StopIteration:
            pass

    def __parse_tag(self, elem, tag_content):
        for mat in attrib_re.finditer(tag_content):
            key = mat.group(1)
            elem.attribs[key] = mat.group(2)
            elem.attrib_keys.append(key)

    def write(self, buf, translator=None):
        if self.tag:
            buf.write('<')
            buf.write(self.tag)

            for key in self.attrib_keys:
                if key in ('eid', 'aid', 'rid'):
                    continue
                buf.write(' ')
                buf.write(key)
                buf.write('="')
                if translator:
                    buf.write(translator(self.attribs[key]))
                else:
                    buf.write(self.attribs[key])
                buf.write('"')

            if self.closed:
                buf.write(' />')
            else:
                buf.write('>')

        for child in self.children:
            child.write(buf, translator)

        if self.tag and not self.closed:
            buf.write('</')
            buf.write(self.tag)
            buf.write('>')


def find_last_matching_elem(elem, idtype, idval, match=None):
    if elem is not None:
        if elem.attribs and idtype in elem.attribs and elem.attribs[idtype] == idval:
            match = elem

        if elem.children:
            for child in elem.children:
                rtn = find_last_matching_elem(child, idtype, idval, match)
                if rtn:
                    match = rtn
    return match


class TextElement(object):
    def __init__(self, content):
        self.content = content
        self.tag = None
        self.attribs = None
        self.children = None

    def __str__(self):
        s = self.content.rstrip()
        if s == '':
            s = ' '
        return 'text("%s")' % s

    def write(self, buf, translator=None):
        if translator:
            buf.write(translator(str(self.content)))
        else:
            buf.write(str(self.content))


class Template(object):
    class_cache = {}

    def __init__(self, path):
        self.path = path
        self.__element_ids = {}
        self.__attrib_ids = {}
        self.__repeat_ids = {}
        self.root = Element(None)
        with open(self.path) as f:
            content = f.read()
            self.root.parse(content, self)
        self.translator = None

    def check_element(self, elem, check_children=False, next_to_elem=None):
        """
        Given an element, check its attributes for references to the three proton attributes ('eid', 'aid' and 'rid').
        """
        self.__add_element('eid', elem.attribs, self.__element_ids, elem, next_to_elem)
        self.__add_element('aid', elem.attribs, self.__attrib_ids, elem, next_to_elem)
        self.__add_element('rid', elem.attribs, self.__repeat_ids, elem, next_to_elem)
        if check_children and elem.children:
            for child in elem.children:
                self.check_element(child, True, next_to_elem)

    def __add_element(self, idtype, attribs, attribmap, elem, next_to_elem=None):
        if attribs and idtype in attribs:
            idval = attribs[idtype]
            if idval not in attribmap:
                attribmap[idval] = []
            lst = attribmap[idval]

            matching_elem = find_last_matching_elem(next_to_elem, idtype, attribs[idtype])
            if matching_elem and matching_elem in lst:
                pos = lst.index(matching_elem)
                lst.insert(pos + 1, elem)
            else:
                lst.append(elem)

    def append(self, eid, content, idx='*'):
        if eid in self.__element_ids:
            elems = self.__element_ids[eid]
            if idx == '*':
                for elem in elems:
                    self.__append(elem, content)
            elif idx < len(elems):
                self.__append(elems[idx], content)

    def __append(self, elem, content):
        elem.parse(content, self)

    def set_value(self, eid, val, idx='*'):
        """
        Set the content of an xml element marked with the matching eid attribute.
        """
        if eid in self.__element_ids:
            elems = self.__element_ids[eid]
            if type(val) in SEQ_TYPES:
                idx = 0
            if idx == '*':
                for elem in elems:
                    self.__set_value(eid, elem, val, idx)
            elif idx < len(elems):
                self.__set_value(eid, elems[idx], val, idx)

    def __set_value(self, eid, elem, val, idx):
        if type(val) in SEQ_TYPES:
            if idx == '*':
                idx = 0
            self.__repeat(elem, len(val))
            for x in range(0, len(val)):
                self.set_value(eid, val[x], idx + x)
        else:
            elem.children = [TextElement(val)]

    def set_attribute(self, aid, attrib, val, idx='*'):
        """
        Set the value of an xml attribute marked with the matching aid attribute.
        """
        if aid in self.__attrib_ids:
            elems = self.__attrib_ids[aid]
            if idx == '*':
                for elem in elems:
                    self.__set_attribute(elem, attrib, val)
            elif idx < len(elems):
                elem = elems[idx]
                self.__set_attribute(elem, attrib, val)

    def __set_attribute(self, elem, attrib, val):
        if attrib not in elem.attribs:
            elem.attrib_keys.append(attrib)
        elem.attribs[attrib] = val

    def set_properties(self, eid, value, idx='*'):
        """
        Set the value and/or attributes of an xml element, marked with the matching eid attribute, using the
        properties of the specified object.
        """
        if value.__class__ not in Template.class_cache:
            props = []
            for name in dir(value.__class__):
                prop = getattr(value.__class__, name)
                if type(prop) == property and hasattr(prop, 'fget'):
                    props.append((name, prop))
            Template.class_cache[value.__class__] = props
        for (name, prop) in Template.class_cache[value.__class__]:
            new_eid = ''.join([eid, ':', name])
            self.set_value(new_eid, prop.fget(value), idx)
            self.set_attribute(eid, name, prop.fget(value), idx)

    def hide(self, eid, index=0):
        """
        Hide the element with the matching eid. If no match, look for an element with a matching rid.
        """
        elems = None
        if eid in self.__element_ids:
            elems = self.__element_ids[eid]
        elif eid in self.__repeat_ids:
            elems = self.__repeat_ids[eid]

        if elems and index < len(elems):
            elem = elems[index]
            elem.parent.children.remove(elem)

    def repeat(self, rid, count, index=0):
        """
        Repeat an xml element marked with the matching rid.
        """
        elems = None
        if rid in self.__repeat_ids:
            elems = self.__repeat_ids[rid]
        elif rid in self.__element_ids:
            elems = self.__element_ids

        if elems and index < len(elems):
            elem = elems[index]
            self.__repeat(elem, count)

    def __repeat(self, elem, count):
        current_pos = elem.parent.children.index(elem) + 1
        copied_elem = elem
        for x in range(0, count - 1):
            elem_copy = copy.deepcopy(elem)
            self.check_element(elem_copy, True, copied_elem)
            copied_elem = elem_copy
            elem.parent.children.insert(current_pos + x, elem_copy)

    def replace(self, eid, replacement, index=0):
        """
        Replace an xml element marked with the matching eid. If the replacement value is an Element or TextElement,
        it's swapped in untouched. If it's a Template, the children of the root element in the template are used.
        Otherwise the replacement value is wrapped with a TextElement.
        """
        if eid in self.__element_ids:
            elems = self.__element_ids[eid]
        elif eid in self.__repeat_ids:
            elems = self.__repeat_ids[eid]
        else:
            return

        if index < len(elems):
            elem = elems[index]
            current_pos = elem.parent.children.index(elem)
            elem.parent.children.remove(elem)
            replacement_type = type(replacement)
            if replacement_type in (Element, TextElement):
                self.check_element(replacement, True)
                elem.parent.children.insert(current_pos, replacement)
                replacement.parent = elem.parent
            elif replacement_type == Template:
                for child in replacement.root.children:
                    elem.parent.children.insert(current_pos, child)
                    child.parent = elem.parent
                    current_pos += 1
                self.__merge_ids(self.__element_ids, replacement.__element_ids)
                self.__merge_ids(self.__attrib_ids, replacement.__attrib_ids)
                self.__merge_ids(self.__repeat_ids, replacement.__repeat_ids)
            else:
                elem.parent.children.insert(current_pos, TextElement(replacement))

    def __merge_ids(self, self_ids, ids):
        for key, val in ids.items():
            if key in self_ids:
                self_ids[key] += val
            else:
                self_ids[key] = val

    def __str__(self):
        buf = io.StringIO()
        self.root.write(buf, self.translator)
        return buf.getvalue()


base_dir = os.getcwd()
templates = {}


def get_template(name):
    """
    Return a copy of the template with the specified name. If not found, or an error occurs
    during the load, return None.
    """
    path = os.path.join(base_dir, name)

    if path not in templates:
        try:
            templates[path] = Template(path)
        except IOError:
            return None

    return copy.deepcopy(templates[path])
