#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Utility functions for mtspec. Mainly useful for testing and playing around.

:copyright:
    Lion Krischer (krischer@geophysik.uni-muenchen.de) and
    Moritz Beyreuther, 2010-2015
:license:
    GNU General Public License, Version 3
    (http://www.gnu.org/copyleft/gpl.html)
"""
import ctypes as C
import gzip
import numpy as np
import os
import platform


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


def _load_mtdata(gzfile):
    """
    Simple helper function that finds the test data in the directory tree
    and loads it using :func:`gzip.open` and :func:`numpy.loadtxt`.

    :param gzfile: Filename
    :type gzfile: str
    :returns: data
    :rtype: numpy.ndarray
    """
    path = os.path.join(os.path.dirname(__file__), 'tests', 'data', gzfile)
    return np.loadtxt(gzip.open(path))


def signal_bursts():
    """
    Generates a signal with two bursts inside. Useful for testing time
    frequency distributions.

    :returns: Generated signal
    :rtype: numpy.ndarray
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


def linear_chirp(npts=2000):
    """
    Generates a simple linear chirp.

    :param npts: Number of samples.
    :type npts: int
    :returns: Generated signal
    :rtype: numpy.ndarray
    """
    time = np.linspace(0, 20, npts)
    chirp = np.sin(0.2 * np.pi * (0.1 + 24.0 / 2.0 * time) * time)
    return chirp


def exponential_chirp(npts=2000):
    """
    Generates an exponential chirp.

    :param npts: Number of samples.
    :type npts: int
    :returns: Generated signal
    :rtype: numpy.ndarray
    """
    time = np.linspace(0, 20, npts)
    chirp = np.sin(2 * np.pi * 0.2 * (1.3 ** time - 1) / np.log(1.3))
    return chirp
