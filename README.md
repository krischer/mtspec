# mtspec - Multitaper Spectral Estimation

[![Build Status](https://travis-ci.org/krischer/mtspec.svg?branch=master)](https://travis-ci.org/krischer/mtspec) [![PyPI Version](https://img.shields.io/pypi/v/mtspec.svg)](https://pypi.python.org/pypi/mtspec) [![Supported Python versions](https://img.shields.io/pypi/pyversions/mtspec.svg)](https://pypi.python.org/pypi/mtspec/) [![License](https://img.shields.io/pypi/l/mtspec.svg)](https://pypi.python.org/pypi/mtspec/)

[Documentation](http://krischer.github.io/mtspec/)

![Logo](http://krischer.github.io/mtspec/_static/logo.svg)

## About mtspec

**mtspec** is a Python (ctypes) wrapper for the [Multitaper Spectrum Estimation
Library mwlib.a](http://wwwprof.uniandes.edu.co/~gprieto/software/mwlib.html)
by Germán A. Prieto.

```
Prieto, G. A., R. L. Parker, F. L. Vernon. (2009),
A Fortran 90 library for multitaper spectrum analysis,
Computers and Geosciences, 35, pp. 1701-1710.
doi:10.1016/ j.cageo.2008.06.007
```

It enables you to calculate Slepian windows, perform multitaper spectral
estimations with various options, calculate Wigner-Ville time-frequency
distributions, and construct coherence spectra with multitapers.
