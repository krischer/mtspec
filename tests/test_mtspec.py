#!/usr/bin/env python

import numpy as np
import ctypes as C
import matplotlib.pyplot as plt
lib = C.CDLL('../lib/mtspec.so')

def mtspec(data, df, tbp=3.5, kspec=5, nf=-1):
    """
    mtspec Python wrapper
    """
    if nf == -1:
        nf = int(round(.5 * len(data)))
    dt = 1/float(df)
    data = np.require(data, 'float64', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])
    freq = np.require(np.zeros(nf), 'float64', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])
    spec = np.require(np.zeros(nf), 'float64', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])
    lib.mtspec_d_(C.byref(C.c_int(len(data))), C.byref(C.c_double(dt)),
                  data.ctypes.data_as(C.POINTER(C.c_double)),
                  C.byref(C.c_double(tbp)),
                  C.byref(C.c_int(kspec)),
                  C.byref(C.c_int(nf)),
                  freq.ctypes.data_as(C.POINTER(C.c_double)),
                  spec.ctypes.data_as(C.POINTER(C.c_double)),
                  None, None, None, None, None,
                  None, None, None, None, None, 
                  None)
    return (freq, spec)

data = np.random.random(512)
df = 10

#from obspy.core import read
#st = read("BW.BGLD..EHE.D.2008.001.first_10_percent")
#data = st[0].data[:100000]
#df = st[0].stats.sampling_rate

x,y = mtspec(data, df, nf=100)

plt.subplot(2,1,1)
plt.plot(np.arange(len(data))/float(df), data)
plt.subplot(2,1,2)
plt.plot(x,y)
plt.show()
