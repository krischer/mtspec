from obspy.core import read
from mtspec import mtspec
import numpy as np
import time
import gzip
import os

datafile = os.path.join(os.path.dirname(__file__), 'data',
                        'PASC.dat.gz')
data = np.loadtxt(gzip.open(datafile))
delta = 1.0

# Level and demean data.
# data -= data.mean()
# data -= np.linspace(data[0], data[-1],
#                     len(data))

print 'Calling once to save the DPSS to not bias the test results...'
print '================================================='
# The Fortran library saves the DPSS in one session. This are the longest
# calculations.
a = time.time()
stuff = mtspec(data, delta, 4)
b = time.time()
c = b - a

a = time.time()
stuff = mtspec(data, delta, 4)
b = time.time()
d = c - (b-a)
print 'Time for mtspec without any optional stuff:', b-a
print 'Approximated time for "Cold call":', d + (b-a)
print '================================================='

a = time.time()
stuff = mtspec(data, delta, 4, optional_output = True)
b = time.time()
print 'Time for mtspec with optional output:', b-a
print 'Approximated time for "Cold call":', d + (b-a)
print '================================================='

a = time.time()
stuff = mtspec(data, delta, 4, statistics = True)
b = time.time()
print 'Time for mtspec with statistics:', b-a
print 'Approximated time for "Cold call":', d + (b-a)
print '================================================='

a = time.time()
stuff = mtspec(data, delta, 4, optional_output = True,
               statistics = True)
b = time.time()
print 'Time for mtspec with optional stuff and statistics:', b-a
print 'Approximated time for "Cold call":', d + (b-a)
