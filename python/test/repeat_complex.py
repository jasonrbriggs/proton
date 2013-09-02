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


class TestAdvancedFunctionality(unittest.TestCase):

    def setUp(self):
        template.base_dir = os.path.dirname(os.path.realpath(__file__))

    def test_repeat(self):
        tmp = template.get_template('repeat-complex.xhtml')

        tmp.repeat('posts', 5)

        # first post
        tmp.set_value('content', 'lorum ipsom 1', 0)
        tmp.repeat('taglinks', 1, 0)
        tmp.set_value('taglink', 'testtag1', 0)
        tmp.set_attribute('taglink', 'href', '/tags/testtag1', 0)

        # second post
        tmp.set_value('content', 'lorum ipsom 2', 1)
        tmp.repeat('taglinks', 1, 1)
        tmp.set_value('taglink', 'testtag2', 1)
        tmp.set_attribute('taglink', 'href', '/tags/testtag2', 1)

        # third post
        tmp.set_value('content', 'lorum ipsom 3', 2)
        tmp.repeat('taglinks', 2, 2)
        tmp.set_value('taglink', 'testtag3', 2)
        tmp.set_attribute('taglink', 'href', '/tags/testtag3', 2)
        tmp.set_value('taglink', 'testtag4', 3)
        tmp.set_attribute('taglink', 'href', '/tags/testtag4', 3)

        # fourth post
        tmp.set_value('content', 'lorum ipsom 4', 3)
        tmp.repeat('taglinks', 3, 4)
        tmp.set_value('taglink', 'testtag5', 4)
        tmp.set_attribute('taglink', 'href', '/tags/testtag5', 4)
        tmp.set_value('taglink', 'testtag6', 5)
        tmp.set_attribute('taglink', 'href', '/tags/testtag6', 5)
        tmp.set_value('taglink', 'testtag7', 6)
        tmp.set_attribute('taglink', 'href', '/tags/testtag7', 6)

        # fifth post
        tmp.set_value('content', 'lorum ipsom 5', 4)
        tmp.repeat('taglinks', 2, 7)
        tmp.set_value('taglink', 'testtag8', 7)
        tmp.set_attribute('taglink', 'href', '/tags/testtag8', 7)
        tmp.set_value('taglink', 'testtag9', 8)
        tmp.set_attribute('taglink', 'href', '/tags/testtag9', 8)

        out = str(tmp)
        print(out)