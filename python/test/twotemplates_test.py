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


class TestTwoTemplatesFunctionality(unittest.TestCase):

    def setUp(self):
        template.base_dir = os.path.dirname(os.path.realpath(__file__))

    def applydata(self, tmp):
        tmp.repeat('list', 2)
        items = [ 'A', 'B' ]
        for x in range(0, 2):
            y = x + 1
            tmp.set_value('listid', str(y), x)
            tmp.set_attribute('listid', 'id', str(y), x)
            tmp.set_value('listval', 'my item %s' % items[x], x)

    def test_two_templates(self):
        tmp1 = template.get_template('twotemplates.xhtml')
        self.applydata(tmp1)
        print("\nXHTML:\n%s" % str(tmp1))

        et = etree.fromstring(str(tmp1))
        td = et.findall('.//td')
        self.assert_(td[1].text == '1', 'expected 1 was %s' % td[1].text)
        self.assert_(td[3].text == 'my item A')
        self.assert_(td[5].text == '2')
        self.assert_(td[7].text == 'my item B')

        tmp2 = template.get_template('twotemplates.xml')
        self.applydata(tmp2)
        print("\nXML:\n%s\n" % str(tmp2))
        
        et = etree.fromstring(str(tmp2))
        item = et.findall('item')
        self.assert_(item[0].attrib['id'] == '1')
        self.assert_(item[0].text == 'my item A')
        self.assert_(item[1].attrib['id'] == '2')
        self.assert_(item[1].text == 'my item B')
    
