**Version 0.3.0a: October 4th, 2012**
    - Move from ctypes to f2py to be more compatible with different build systems and less error-prone.
    - New way to call mtspec.dpss() - the old one will not work anymore:
        `mtspec.dpss(npts, tbw_prod, number_of_tapers, spline_interpolation=False)`


**Version 0.2.6: until October 4th, 2012**
    - Working old version using ctypes.
