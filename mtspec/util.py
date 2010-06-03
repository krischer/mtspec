# -*- coding: utf-8 -*-
"""
Loads the mtspec library depending on the platform.
"""

import numpy as np
import ctypes as C
import platform
import os
import gzip


# Import shared mtspec library depending on the platform. 
# Find library name generated via "python setup.py build"
if platform.system() == 'Windows':
    lib_name = 'mtspec.pyd'
elif platform.system() == 'Darwin':
    lib_name = 'mtspec.so'
else:
    lib_name = 'mtspec.so'

# Initialize library
mtspeclib = C.CDLL(os.path.join(os.path.dirname(__file__), 'lib',
                                lib_name))

def load_mtdata(gzfile):
    """
    Helper function that finds the data in the directory tree and loads it
    using `gzip.open` and `np.loadtxt`

    :param gzfile: String of filename, either `v22_174_series.dat.gz` or
        `PASC.dat.gz`
    :returns: numpy.ndarray 1dim, containing the data
    """
    path = os.path.join(os.path.dirname(__file__), 'tests', 'data', gzfile)
    return np.loadtxt(gzip.open(path))
