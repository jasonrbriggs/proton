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

fr = {
    'Page Title' : 'Titre de la page',
    'Some translated text' : 'Certains textes traduits'
}

def translate(text):
    if text in fr:
        return fr[text]
    else:
        return text


class TestI18NFunctionality(unittest.TestCase):

    def setUp(self):
        self.templates = Templates()

    def testtranslation(self):
        tmp = self.templates['test/i18n.xhtml']
        tmp.translate = translate
        
        out = str(tmp)
        print(out)
        
        et = etree.fromstring(out)
        
        self.assert_(et.find('head/title').text == 'Titre de la page', 'the title should be translated')
        self.assert_(et.find('body/h1').text == 'Titre de la page', 'the heading should be translated')
        paras = et.findall('body/p')
        self.assert_(paras[0].text == 'Certains textes traduits', 'para1 should be translated')
        self.assert_(paras[1].text == 'Not translated text', 'para2 should not be translated')

suite = unittest.TestLoader().loadTestsFromTestCase(TestI18NFunctionality)
unittest.TextTestRunner(verbosity=2).run(suite)