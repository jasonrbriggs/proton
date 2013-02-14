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


class TestTwoTemplatesFunctionality(unittest.TestCase):

    def setUp(self):
        self.templates = Templates()

    def applydata(self, tmp):
        tmp.repeat('list', 2)
        for x in range(0, 2):
            y = x + 1
            tmp.setelement('listid', str(y), x)
            tmp.setattribute('listid', 'id', str(y), x)
            tmp.setelement('listval', 'my item %s' % y, x)

    def testrepeat(self):
        tmp1 = self.templates['test/twotemplates.xhtml']
        self.applydata(tmp1)
        print("\nXHTML:\n%s" % str(tmp1))

        et = etree.fromstring(str(tmp1))
        td = et.findall('body/table/tr/td')
        self.assert_(td[1].text == '1')
        self.assert_(td[3].text == 'my item 1')
        self.assert_(td[5].text == '2')
        self.assert_(td[7].text == 'my item 2')

        tmp2 = self.templates['test/twotemplates.xml']
        self.applydata(tmp2)
        print("\nXML:\n%s\n" % str(tmp2))
        
        et = etree.fromstring(str(tmp2))
        item = et.findall('item')
        self.assert_(item[0].attrib['id'] == '1')
        self.assert_(item[0].text == 'my item 1')
        self.assert_(item[1].attrib['id'] == '2')
        self.assert_(item[1].text == 'my item 2')
    

suite = unittest.TestLoader().loadTestsFromTestCase(TestTwoTemplatesFunctionality)
unittest.TextTestRunner(verbosity=2).run(suite)