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


class TestRepeatingFunctionality(unittest.TestCase):

    def setUp(self):
        self.templates = Templates()

    def testrepeat(self):
        tmp = self.templates['test/repeat.xhtml']

        tmp.setelement('title', 'Repeating Xhtml Page', '*')
        tmp.setelement('link', 'This is a link to Google')
        tmp.setattribute('link', 'href', 'http://www.google.com')
        
        tmp.repeat('list-item', 5)
        for x in range(0, 5):
            tmp.setelement('list-item', 'test%s' % x, x)

        out = str(tmp)
        print(out)

        et = etree.fromstring(out)
        li = et.findall('body/ul/li')
        self.assert_(len(li) == 5)
        for x in range(0, 5):
            self.assert_(li[x].text == 'test%s' % x, 'expecting test%s, actual %s' % (x, li[x].text))
        
suite = unittest.TestLoader().loadTestsFromTestCase(TestRepeatingFunctionality)
unittest.TextTestRunner(verbosity=2).run(suite)