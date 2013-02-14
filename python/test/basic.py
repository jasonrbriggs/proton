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
from lxml import etree
from proton.template import Templates


class TestBasicFunctionality(unittest.TestCase):

    def setUp(self):
        self.templates = Templates()

    def testbasic(self):
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


suite = unittest.TestLoader().loadTestsFromTestCase(TestBasicFunctionality)
unittest.TextTestRunner(verbosity=2).run(suite)