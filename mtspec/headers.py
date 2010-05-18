# -*- coding: utf-8 -*-
"""
Loads the mtspec library depending on the platform.
"""

import ctypes as C
import platform
import os

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
