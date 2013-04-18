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

class Ref(object):
    def __init__(self, href, text):
        self._href = href
        self._text = text
        
    @property
    def href(self):
        return self._href
        
    @property 
    def text(self):
        return self._text
    
class Post(object):
    def __init__(self, title, author, date, content):
        self._title = title
        self._author = author
        self._date = date
        self._content = content.split('\n')
        
    @property
    def title(self):
        return self._title
        
    @property
    def author(self):
        return self._author
        
    @property
    def date(self):
        return self._date
        
    @property
    def content(self):
        return self._content

class TestBlogExample(unittest.TestCase):

    def setUp(self):
        template.base_dir = os.path.dirname(os.path.realpath(__file__))

    def get_menu(self):
        return [
            Ref('/home', 'Home'),
            Ref('/about', 'About'),
            Ref('/admin', 'Admin')
        ]
        
    def get_blogroll(self):
        return [
            Ref('http://www.someblog.com', 'Some blog'),
            Ref('http://www.anotherblog.com', 'Another blog')
        ]

    def test_blogrender(self):
        tmp = template.get_template('blogexample.xhtml')
        
        tmp.set_value('title', 'My Test Blog')

        menu = self.get_menu()
        tmp.repeat('menu', len(menu))
        for x in range(0, len(menu)):
            tmp.set_properties('menu', menu[x], x)

        blogroll = self.get_blogroll()
        tmp.repeat('blogroll', len(blogroll))
        for x in range(0, len(blogroll)):
            tmp.set_properties('blogroll', blogroll[x], x)
        
        post = Post('My First Post', 'joe@bloggs.com', '10-Jan-2009 21:47pm', 'This is my first post...\n...and what a great post it is.')
        
        tmp.set_properties('post', post)
        
        out = str(tmp)
        print(out)
        
        et = etree.fromstring(out)
        self.assert_(et.find('body/div/h1').text == 'My First Post')
        p = et.findall('body/div/p/span')
        self.assert_(p[0].text == '10-Jan-2009 21:47pm')
        self.assert_(p[1].text == 'joe@bloggs.com')
        p = et.findall('body/div/div/p')
        self.assert_(p[0].text == 'This is my first post...')
        self.assert_(p[1].text == '...and what a great post it is.')
        
        a = et.findall('body/div/ul/li/a')
        self.assert_(a[0].text == 'Home')
        self.assert_(a[0].attrib['href'] == '/home')
        self.assert_(a[1].text == 'About')
        self.assert_(a[1].attrib['href'] == '/about')
        self.assert_(a[2].text == 'Admin')
        self.assert_(a[2].attrib['href'] == '/admin')
        
        a = et.findall('body/div/ol/li/a')
        self.assert_(a[0].text == 'Some blog')
        self.assert_(a[0].attrib['href'] == 'http://www.someblog.com')
        self.assert_(a[1].text == 'Another blog')
        self.assert_(a[1].attrib['href'] == 'http://www.anotherblog.com')

