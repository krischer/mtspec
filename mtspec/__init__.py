# -*- coding: utf-8 -*-
"""
mtspec - Multitaper Library (mtspec) Python Bindings
====================================================
Wrapper for the multitaper ``mtspec`` library of German A. Prieto,
http://wwwprof.uniandes.edu.co/~gprieto/software/mwlib.html.

Available functions:

* :func:`mtspec.mtspec`: Estimates the adaptive weighted multitaper spectrum
* :func:`mtspec.sine_psd`: Estimating the adaptive sine multitaper

These are the two major functions of the library and are documented below.
The remaining functions can be easily accessed using python ctypes. See the
source code of multitaper.py for examples.
"""

from multitaper import mtspec, sine_psd
