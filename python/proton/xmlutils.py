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

from xml.etree import ElementTree as etree

def index(elem):
    '''
    Return the index position of an element in the children of a parent.
    '''
    parent = elem.getparent()
    for x in range(0, len(parent.getchildren())):
        if parent.getchildren()[x] == elem:
            return x
    return -1


def replaceelement(oldelem, newelem):
    '''
    Given a parent element, replace oldelem with newelem.
    '''
    parent = oldelem.getparent()
    if parent is not None:
        size = len(parent.getchildren())
        for x in range(0, size):
            if parent.getchildren()[x] == oldelem:
                parent.remove(oldelem)
                parent.insert(x, newelem)

            
def parseelement(elem):
    '''
    Convert the content of an element into more ElementTree structures.
    We do this because sometimes we want to set xml as the content of an element.
    '''
    xml = '<%(tag)s>%(content)s</%(tag)s>' % {'tag' : elem.tag, 'content' : elem.text}
    et = etree.fromstring(xml)
    replaceelement(elem, et)
    

def getparent(self):
    return self.parent
    
def loadparents(elem, parent):
    elem.parent = parent
    for parent in elem.getiterator():
        for child in parent:
            loadparents(child, parent)