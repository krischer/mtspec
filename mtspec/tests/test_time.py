from obspy.core import read
from mtspec import mtspec
import numpy as np
import time
import os

st = read(os.path.join('data','test_file.gse2'))
st[0].data = np.require(st[0].data, 'float32')

# Level and demean data.
st[0].data -= st[0].data.mean()
st[0].data -= np.linspace(st[0].data[0], st[0].data[-1],
                          len(st[0].data))

print 'Calling once to save the DPSS to not bias the test results...'
print '================================================='
# The Fortran library saves the DPSS in one session. This are the longest
# calculations.
a = time.time()
stuff = mtspec(st[0].data, st[0].stats.delta, 4)
b = time.time()
c = b - a

a = time.time()
stuff = mtspec(st[0].data, st[0].stats.delta, 4)
b = time.time()
d = c - (b-a)
print 'Time for mtspec without any optional stuff:', b-a
print 'Approximated time for "Cold call":', d + (b-a)
print '================================================='

a = time.time()
stuff = mtspec(st[0].data, st[0].stats.delta, 4, optional_output = True)
b = time.time()
print 'Time for mtspec with optional output:', b-a
print 'Approximated time for "Cold call":', d + (b-a)
print '================================================='

a = time.time()
stuff = mtspec(st[0].data, st[0].stats.delta, 4, statistics = True)
b = time.time()
print 'Time for mtspec with statistics:', b-a
print 'Approximated time for "Cold call":', d + (b-a)
print '================================================='

a = time.time()
stuff = mtspec(st[0].data, st[0].stats.delta, 4, optional_output = True,
               statistics = True)
b = time.time()
print 'Time for mtspec with optional stuff and statistics:', b-a
print 'Approximated time for "Cold call":', d + (b-a)
