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

class TestHidingFunctionality(unittest.TestCase):

    def setUp(self):
        self.templates = Templates()

    def testhiding(self):
        tmp = self.templates['test/hiding.xhtml']
        tmp.setelement('title', 'Hiding Xhtml Page', '*')
        tmp.hide('hidden-element')

        out = str(tmp)
        print(out)
        et = etree.fromstring(out)

        self.assert_(et.find('body/div') == None, 'div should have been removed')
        
    def testhiding2(self):
        tmp = self.templates['test/hiding2.xhtml']
        tmp.setelement('title', 'Navigation Example', '*')
        tmp.hide('autopayments')
        tmp.hide('exchange')
        tmp.hide('transactions')
        
        out = str(tmp)
        print(out)
        et = etree.fromstring(out)

        anchors = et.findall('body/ul/li/a[@href]')
        self.assert_(len(anchors) == 3, 'there should be only 3 anchor elements, found %s' % len(anchors))
        self.assert_(anchors[0].attrib['href'] == '/accounts')
        self.assert_(anchors[1].attrib['href'] == '/transfer')
        self.assert_(anchors[2].attrib['href'] == '/bills')
        
