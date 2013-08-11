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


class TestReplaceFunctionality(unittest.TestCase):

    def setUp(self):
        template.base_dir = os.path.dirname(os.path.realpath(__file__))

    def test_replace_eid(self):
        tmp = template.get_template('replace.xhtml')

        tmp.replace('title', '<h2>replaced title</h2>')

        out = str(tmp)
        print(out)

        et = etree.fromstring(out)
        h2 = et.findall('body/h2')
        self.assert_(len(h2) == 1)

    def test_replace_rid(self):
        tmp = template.get_template('replace.xhtml')

        tmp.replace('list-item', '<p>replaced list item</p>')

        out = str(tmp)
        print(out)

        et = etree.fromstring(out)
        p = et.findall('body/ul/p')
        self.assert_(len(p) == 1)
