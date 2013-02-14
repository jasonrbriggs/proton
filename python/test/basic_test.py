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
from proton.template import Templates


class TestBasicFunctionality(unittest.TestCase):

    def setUp(self):
        self.templates = Templates()

    def test_basic(self):
        tmp = self.templates['test/basic.xhtml']
        tmp.setelement('title', 'Basic Xhtml Page', '*')
        tmp.setelement('content', 'Content goes here')
        tmp.setelement('link', 'Link goes here')
        tmp.setattribute('link', 'href', 'http://www.google.com')
        
        out = str(tmp)
        print(out)
        et = etree.fromstring(out)

        self.assert_(et.find('head/title').text == 'Basic Xhtml Page', 'incorrect title')
        self.assert_(et.find('body/h1').text == 'Basic Xhtml Page', 'incorrect heading')
        self.assert_(et.find('body/div').text == 'Content goes here', 'incorrect body')
        self.assert_(et.find('body/a').text == 'Link goes here', 'incorrect anchor text')
        self.assert_(et.find('body/a').attrib['href'] == 'http://www.google.com', 'incorrect anchor href')

    def test_basic_twice(self):
        tmp = self.templates['test/basic.xhtml']
        tmp.setelement('title', 'Basic Xhtml Page', '*')

        out1 = str(tmp)
        et1 = etree.fromstring(out1)

        tmp = self.templates['test/basic.xhtml']
        tmp.setelement('title', 'Basic Xhtml Page 2', '*')

        out2 = str(tmp)
        et2 = etree.fromstring(out2)

        self.assert_(et1.find('head/title').text == 'Basic Xhtml Page', 'incorrect title')
        self.assert_(et2.find('head/title').text == 'Basic Xhtml Page 2', 'incorrect title')

    def test_basic_with_namespace(self):
        tmp = self.templates['test/basic-with-namespace.xml']

        tmp.repeat('urls', 2)

        tmp.setelement('url', 'http://testhost/test1.html', 0)
        tmp.setelement('last-modified', '2012-01-01T23:59:59', 0)

        tmp.setelement('url', 'http://testhost/test2.html', 1)
        tmp.setelement('last-modified', '2012-01-02T23:59:59', 1)

        out = str(tmp)
