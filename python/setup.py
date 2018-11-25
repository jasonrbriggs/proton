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

from setuptools import setup, Command
import proton
import unittest
from Cython.Build import cythonize

class TestCommand(Command):
    user_options = [ ('test=', 't', 'specific test to run') ]

    def initialize_options(self):
        self.test = '*'

    def finalize_options(self):
        pass

    def run(self):
        suite = unittest.TestSuite()
        if self.test == '*':
            import test
            for tst in test.__all__:
                suite.addTests(unittest.TestLoader().loadTestsFromName('test.%s' % tst))
        else:
            suite = unittest.TestLoader().loadTestsFromName('test.%s' % self.test)
        unittest.TextTestRunner(verbosity=2).run(suite)


setup(
    name = 'proton',
    version = proton.__version__,
    description = 'Proton Template Engine',
    license = 'LGPL',
    url = 'https://github.com/jasonrbriggs/proton',
    author = 'Jason R Briggs',
    author_email =  'jasonrbriggs@gmail.com',
    platforms = ['3.0', '3.1', '3.2', '3.3'],
    packages = ['proton', 'test'],
    requires = 'more_iterutils',
    cmdclass = {'test': TestCommand}
)
