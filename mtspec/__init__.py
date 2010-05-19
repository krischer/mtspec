# -*- coding: utf-8 -*-
"""
mtspec - Multitaper Library (mtspec) Python Bindings
====================================================
Wrapper for the multitaper ``mtspec`` library of German A. Prieto.

http://wwwprof.uniandes.edu.co/~gprieto/software/mwlib.html

Available functions::

* mtspec
* mtspec_pad
* sine_psd

This are the two major functions of the library. The remaining functions
can be easily accessed using python ctypes. See the source code of
multitaper.py for examples.
"""
from multitaper import mtspec, sine_psd, mtspec_pad
