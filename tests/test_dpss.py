#!/usr/bin/env python

import numpy as np
import ctypes as C
import matplotlib.pyplot as plt
lib = C.CDLL('../lib/mtspec.so')

xx = np.random.random((512,2))
xx = np.require(xx, 'float64', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])

yy = np.random.random(2)
yy = np.require(yy, 'float64', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])

zz = np.random.random(2)
zz = np.require(zz, 'float64', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])



lib.dpss_(C.byref(C.c_int(512)), C.byref(C.c_double(2.5)), 
          C.byref(C.c_int(2)), 
          xx.ctypes.data_as(C.POINTER(C.c_double)), 
          yy.ctypes.data_as(C.POINTER(C.c_double)), 
          zz.ctypes.data_as(C.POINTER(C.c_double)))

print xx
#plt.plot(xx[:,0])
#plt.plot(xx[:,1])
#plt.show()
