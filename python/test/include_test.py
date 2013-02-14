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

import unittest
from xml.etree import ElementTree as etree
from proton.template import Templates


class TestIncludeFunctionality(unittest.TestCase):

    def setUp(self):
        self.templates = Templates()

    def testinclude(self):
        tmp = self.templates['test/include1.xhtml']
        
        tmp.setelement('title', 'Page Title')
        tmp.include('include-content', 'test/include2.xhtml')
        tmp.setelement('para1', 'First paragraph of text')
        tmp.setelement('para2', 'Second paragraph of text')

        out = str(tmp)
        print(out)

        et = etree.fromstring(out)

        self.assert_(et.find('body/h1').text == 'Page Title', 'incorrect heading')
        p = et.findall('body/div/p')
        self.assert_(len(p) == 2, 'should be 2 paragraphs')
        self.assert_(p[0].text == 'First paragraph of text', 'paragraph text incorrect')
        self.assert_(p[1].text == 'Second paragraph of text', 'paragraph text incorrect')

