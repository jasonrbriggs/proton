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


class TestMagicFunctionality(unittest.TestCase):

    def setUp(self):
        self.templates = Templates()

    def testmagiclist(self):
        tmp = self.templates['test/magic-list.xhtml']
        
        lst = [ 'a', 'b', 'c', 'd', 'e', 'f', 'g' ]
        tmp.setelement('list-item', lst)
        
        out = str(tmp)
        print(out)
        et = etree.fromstring(out)
        li = et.findall('body/ul/li')
        
        self.assert_(li[0].text == 'a')
        self.assert_(li[1].text == 'b')
        self.assert_(li[5].text == 'f')
        self.assert_(li[6].text == 'g')

    def testmagicprops(self):
        tmp = self.templates['test/magic-props.xhtml']
        
        t = Temp('100', '500')
        tmp.setelement('prop', t)
        
        et = etree.fromstring(str(tmp))
        dd = et.findall('body/dl/dd')
        self.assert_(dd[0].text == '100')
        self.assert_(dd[1].text == '500')

