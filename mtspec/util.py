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
    Simple helper function that finds the test data in the directory tree
    and loads it using `gzip.open` and `numpy.loadtxt`

    :param gzfile: String of filename, either `v22_174_series.dat.gz` or
        `PASC.dat.gz`
    :returns: numpy.ndarray 1dim, containing the data
    """
    path = os.path.join(os.path.dirname(__file__), 'tests', 'data', gzfile)
    return np.loadtxt(gzip.open(path))

def signal_bursts():
    """
    Function which returns a signal (numpy.ndarray) with two signal bursts inside.
    """
    np.random.seed(815)
    length = 5 * 512

    # Baseline low frequency plut noise.
    data = np.sin(np.linspace(0, 80 * np.pi, length))
    noise = np.random.ranf(length)
    noise /= noise.max()
    noise /= 15
    data += noise

    # Double last two fifths of the signal.
    data[-2 * 512:] *= 2.0
    chirp1 = 2.5 * np.sin(np.linspace(0, 400 * np.pi, 512))
    chirp1 *= np.linspace(1, 0, 512)
    data[512:2 * 512] += chirp1

    # Add second transient signal.
    chirp2 = 5.0 * np.sin(np.linspace(0, 200 * np.pi, 512))
    chirp2 *= np.linspace(1, 0, 512)
    data[3 * 512:4 * 512] += chirp2

    return data
