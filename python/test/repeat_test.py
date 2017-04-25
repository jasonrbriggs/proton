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


class TestRepeatingFunctionality(unittest.TestCase):

    def setUp(self):
        template.base_dir = os.path.dirname(os.path.realpath(__file__))

    def test_repeat(self):
        tmp = template.get_template('repeat.xhtml')

        tmp.set_value('title', 'Repeating Xhtml Page', '*')
        tmp.set_value('link', 'This is a link to DuckDuckGo')
        tmp.set_attribute('link', 'href', 'http://www.duckduckgo.com')
        
        tmp.repeat('list-item', 5)
        for x in range(0, 5):
            tmp.set_value('list-item', 'test%s' % x, x)

        out = str(tmp)
        print(out)

        et = etree.fromstring(out)
        li = et.findall('body/ul/li')
        self.assert_(len(li) == 5)
        for x in range(0, 5):
            self.assert_(li[x].text == 'test%s' % x, 'expecting test%s, actual %s' % (x, li[x].text))

    def test_repeat2(self):
        tmp = template.get_template('repeat2.xhtml')

        tmp.repeat('posts', 5)

        out = str(tmp)
        print(out)

        et = etree.fromstring(out)
        li = et.findall("*//div[@class='contentcontainer']")
        self.assert_(len(li) == 5, 'expecting 5 content divs, actual %s' % len(li))

        li = et.findall("*//div[@id='menucontainer']")
        self.assert_(len(li) == 1, 'expecting 1 menu divs, actual %s' % len(li))

    def test_repeat3(self):
        tmp = template.get_template('repeat2.xhtml')

        tmp.repeat('posts', 50)

        out = str(tmp)

        et = etree.fromstring(out)
        li = et.findall("*//div[@class='contentcontainer']")
        self.assert_(len(li) == 50, 'expecting 50 content divs, actual %s' % len(li))

        li = et.findall("*//div[@id='menucontainer']")
        self.assert_(len(li) == 1, 'expecting 1 menu divs, actual %s' % len(li))
        
    def test_repeat4(self):
        pg = template.get_template('repeat3.xhtml')
        chunk = [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
        length = len(chunk)
        pg.repeat('image', 20)
        
        print(str(pg))