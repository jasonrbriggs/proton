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
import os
from proton import template

class TestHidingFunctionality(unittest.TestCase):

    def setUp(self):
        template.base_dir = os.path.dirname(os.path.realpath(__file__))

    def test_hiding(self):
        tmp = template.get_template('hiding.xhtml')
        tmp.set_value('title', 'Hiding Xhtml Page', '*')
        tmp.hide('hidden-element')

        out = str(tmp)
        print(out)
        et = etree.fromstring(out)

        self.assert_(et.find('body/div') == None, 'div should have been removed')
        
    def test_hiding2(self):
        tmp = template.get_template('hiding2.xhtml')
        tmp.set_value('title', 'Navigation Example', '*')
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

    def test_hiding3(self):
        tmp = template.get_template('hiding3.xhtml')
        tmp.set_value('title', 'Hiding Xhtml Page', '*')

        tmp2 = template.get_template('hiding-include.xhtml')

        tmp.replace('replaced-element', tmp2)

        tmp.set_value('not-hidden', 'Not hidden content')
        tmp.hide('hidden-element')

        out = str(tmp)
        print(out)
        et = etree.fromstring(out)

        self.assert_(et.find('body/span') == None, 'span should have been removed')

