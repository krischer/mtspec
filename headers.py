# -*- coding: utf-8 -*-
"""
Loads the mtspec library depending on the platform.
"""

import ctypes as C
import platform
import os

# Import shared mtspec library depending on the platform. 
# Trying multiple libraries: 
# First entry: library name generated via "python setup.py build"
# Further entries: pre-generated libraries
if platform.system() == 'Windows':
    if platform.architecture()[0] == '64bit':
        lib_names = ['mtspec.pyd', '_mtspec.win64.dll']
    else:
        lib_names = ['mtspec.pyd', '_mtspec.win32.dll']
elif platform.system() == 'Darwin':
    lib_names = ['mtspec.so', '_mtspec.dylib']
else:
    # 32 and 64 bit UNIX
    #XXX Check glibc version by platform.libc_ver()
    if platform.architecture()[0] == '64bit':
        lib_names = ['mtspec.so', '_mtspec.lin64.so']
    else:
        lib_names = ['mtspec.so', '_mtspec.so']

# initialize library
mtspeclib = None
for lib_name in lib_names:
    try:
        mtspeclib = C.CDLL(os.path.join(os.path.dirname(__file__), 'lib',
                                        lib_name))
    except Exception, e:
        continue
    else:
        break
if not mtspeclib:
    msg = 'Could not load shared library "mtspec" for mtspecpy.' + \
          '\n\n %s' % str(e)
    raise ImportError(msg)
