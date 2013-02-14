import cProfile
import os
import sys
import time
import unittest

from proton.template import Templates

class ListItem(object):
    def __init__(self):
        self.id = None
        self.clazz = None

class Datum(ListItem):
    def __init__(self, cols):
        self.name = cols[0]
        self.description = cols[1]
        self.url = cols[2]
        self.symbol = cols[3]
        self.price = cols[4]
        self.change = cols[5]
        self.ratio = cols[6]

    
data = []    
f = open('test/benchmark-data.csv')
lines = f.readlines()
for x in range(0, len(lines)):
    d = Datum(lines[x].split('\t'))
    data.append(d)
    d.id = x + 1
    d.clazz = x % 2 == 1 and 'odd' or 'even'

templates = Templates()

def run(times):
    ## call benchmark function
    for x in range(0, times):
        tmp = templates['test/benchmark.xhtml']
        tmp.repeat('data', len(data))
        for y in range(0, len(data)):
            d = data[y]
            tmp.setattribute('clazz', 'class', d.clazz, y)
            tmp.setelement('id', str(d.id), y)
            tmp.setattribute('symbol', 'href', '/stock/%s' % d.symbol, y)
            tmp.setelement('symbol', d.symbol, y)
            tmp.setattribute('url', 'href', d.url, y)
            tmp.setelement('name', d.name, y)
            tmp.setelement('price', d.price, y)
            if float(d.change) < 0:
                tmp.hide('azchange', y)
                tmp.hide('azratio', y)
                tmp.setelement('bzchange', d.change, y)
                tmp.setelement('bzratio', d.ratio, y)
            else:
                tmp.hide('bzchange', y)
                tmp.hide('bzratio', y)
                tmp.setelement('azchange', d.change, y)
                tmp.setelement('azratio', d.ratio, y)
        str(tmp)


class TestPerformance(unittest.TestCase):

    def setUp(self):
        self.templates = Templates()

    def testperf(self):
        times = 10000

        ## start time
        start_t = time.time()
        t1 = os.times()

        run(times)

        ## end time
        t2 = os.times()
        end_t = time.time()

        ## result
        utime = t2[0]-t1[0]
        stime = t2[1]-t1[1]
        #total = t2[4]-t1[4]
        total = utime + stime
        real  = end_t-start_t
        print("%10.5f %10.5f %10.5f %10.5f" % (utime, stime, total, real))


suite = unittest.TestLoader().loadTestsFromTestCase(TestPerformance)
unittest.TextTestRunner(verbosity=2).run(suite)