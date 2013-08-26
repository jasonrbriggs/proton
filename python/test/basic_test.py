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
import sys
sys.path.append('..')

import unittest
from xml.etree import ElementTree as etree
import os
from proton import template

class Temp(object):
    def __init__(self, x, y):
        self._x = x
        self._y = y

    @property
    def x(self):
        return self._x

    @property
    def y(self):
        return self._y

class Temp2(object):
    def __init__(self, href):
        self._href = href

    @property
    def href(self):
        return self._href

class TestBasicFunctionality(unittest.TestCase):

    def setUp(self):
        template.base_dir = os.path.dirname(os.path.realpath(__file__))

    def test_basic(self):
        tmp = template.get_template('basic.xhtml')
        tmp.set_value('title', 'Basic Xhtml Page', '*')
        tmp.set_value('content', 'Content goes here')
        tmp.set_value('link', 'Link goes here')
        tmp.set_attribute('link', 'href', 'http://www.google.com')

        out = str(tmp)
        print(out)
        et = etree.fromstring(out)

        self.assert_(et.find('head/title').text == 'Basic Xhtml Page', 'incorrect title')
        self.assert_(et.find('body/h1').text == 'Basic Xhtml Page', 'incorrect heading')
        self.assert_(et.find('body/div').text == 'Content goes here', 'incorrect body')
        self.assert_(et.find('body/a').text == 'Link goes here', 'incorrect anchor text')
        self.assert_(et.find('body/a').attrib['href'] == 'http://www.google.com', 'incorrect anchor href')

    def test_basic_twice(self):
        tmp = template.get_template('basic.xhtml')

        tmp.set_value('title', 'Basic Xhtml Page', '*')

        out1 = str(tmp)
        et1 = etree.fromstring(out1)

        tmp = template.get_template('basic.xhtml')
        tmp.set_value('title', 'Basic Xhtml Page 1', 0)
        tmp.set_value('title', 'Basic Xhtml Page 2', 1)
        tmp.set_value('content', 'Content goes here')
        tmp.set_value('link', 'Link goes here')
        tmp.set_attribute('link', 'href', 'http://www.google.com')

        out2 = str(tmp)
        print(out2)
        et2 = etree.fromstring(out2)

        self.assert_(et1.find('head/title').text == 'Basic Xhtml Page', 'incorrect title')
        self.assert_(et2.find('head/title').text == 'Basic Xhtml Page 1', 'incorrect title')
        self.assert_(et2.find('body/h1').text == 'Basic Xhtml Page 2', 'incorrect title')

    def test_basic_with_namespace(self):
        tmp = template.get_template('basic-with-namespace.xml')

        tmp.repeat('urls', 2)

        tmp.set_value('url', 'http://testhost/test1.html', 0)
        tmp.set_value('last-modified', '2012-01-01T23:59:59', 0)

        tmp.set_value('url', 'http://testhost/test2.html', 1)
        tmp.set_value('last-modified', '2012-01-02T12:59:59', 1)

        out = str(tmp)
        print(out)

    def test_basic_list(self):
        tmp = template.get_template('basic-list.xhtml')

        lst = [ 'a', 'b', 'c', 'd', 'e', 'f', 'g' ]
        tmp.set_value('list-item', lst)

        out = str(tmp)
        print(out)
        et = etree.fromstring(out)
        li = et.findall('body/ul/li')

        self.assert_(li[0].text == 'a')
        self.assert_(li[1].text == 'b')
        self.assert_(li[5].text == 'f')
        self.assert_(li[6].text == 'g')

    def test_basic_props(self):
        tmp = template.get_template('basic-props.xhtml')

        t = Temp('100', '500')
        tmp.set_properties('prop', t)

        t2 = Temp2('http://somewebsite.com')
        tmp.set_properties('prop2', t2)
        tmp.set_value('prop2', 'A Link')

        out = str(tmp)
        print(out)

        et = etree.fromstring(out)
        dd = et.findall('body/dl/dd')
        self.assert_(dd[0].text == '100')
        self.assert_(dd[1].text == '500')

    def test_basic_append(self):
        tmp = template.get_template('basic-append.xhtml')

        tmp.set_value('title', 'Append Title')
        tmp.set_value('content', 'Append Content')

        tmp.append('head', '<meta name="description" content="append description" />')
        tmp.append('content', '<p>some additional content</p>')

        out = str(tmp)
        print(out)

        et = etree.fromstring(out)
        title = et.findall('head/title')
        self.assert_(title[0].text == 'Append Title')
        meta = et.findall('head/meta')
        self.assert_(meta[0].attrib['name'] == 'description')
        self.assert_(meta[0].attrib['content'] == 'append description')

        content = et.findall('body/div')
        self.assert_(content[0].text == 'Append Content')
        appended_content = et.findall('body/div/p')
        self.assert_(appended_content[0].text == 'some additional content')

    def test_basic_nonexistent_eid(self):
        tmp = template.get_template('basic.xhtml')

        tmp.set_value('nonexistent', 'test')

        out = str(tmp)

    def test_basic_nonexistent_aid(self):
        tmp = template.get_template('basic.xhtml')

        tmp.set_attribute('nonexistent', 'href', 'test')

        out = str(tmp)

    def test_basic_nonexistent_rid(self):
        tmp = template.get_template('basic.xhtml')

        tmp.repeat('nonexistent', 10)

        out = str(tmp)