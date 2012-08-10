# mtspec - Python Bindings for multitaper "mtspec" f90 Library by German A. Prieto

We are currently in the process of moving the project to GitHub. It was originally hosted at [https://svn.geophysik.uni-muenchen.de/trac/mtspecpy/wiki]().

### About mtspec
**mtspec** is a Python (ctypes) wrapper for the [Multitaper Spectrum Estimation Library mwlib.a](http://wwwprof.uniandes.edu.co/~gprieto/software/mwlib.html) by Germán A. Prieto.

```
Prieto, G. A., R. L. Parker, F. L. Vernon. (2009),
A Fortran 90 library for multitaper spectrum analysis,
Computers and Geosciences, 35, pp. 1701-1710.
doi:10.1016/ j.cageo.2008.06.007
```

It currently wraps version 3.1 of the library. This is mainly due to later versions using FFTW which would introduce additional dependency.

The following methods are currently wrapped:

* [mtspec](http://krischer.github.com/mtspec/mtspec.multitaper.mtspec.html#mtspec.multitaper.mtspec): Wrapper method for the mtspec subroutine.
* [sine_psd](http://krischer.github.com/mtspec/mtspec.multitaper.sine_psd.html#mtspec.multitaper.sine_psd): Wrapper method for the sine_psd subroutine.
* [wigner_ville_spectrum](http://krischer.github.com/mtspec/mtspec.multitaper.wigner_ville_spectrum.html#mtspec.multitaper.wigner_ville_spectrum): Wrapper method of the modified wv_spec (wv_spec_to_array) subroutine.
* [mt_coherence](http://krischer.github.com/mtspec/mtspec.multitaper.mt_coherence.html#mtspec.multitaper.mt_coherence): Construct the coherence spectrum from the yk’s and the weights of the usual multitaper spectrum estimation.
* [dpss](http://krischer.github.com/mtspec/mtspec.multitaper.dpss.html#mtspec.multitaper.dpss): Wrapper method for the dpss subroutine in the library.

### Documentation and Tutorial
The documentation can be found [here](http://krischer.github.com/mtspec/), together with a short [tutorial](http://krischer.github.com/mtspec/tutorial.html).

### Installation

* Currently runs on Linux (32 and 64bit), Mac (32 and 64bit) and Windows (only 32bit).
* Dependencies: *numpy*, *distribute*, *gfortran* (not necessary for win32)

After cloning the repository, run either of the following two commands to install it via easy_install or pip, respectively:

```bash
python setup.py install
    or
pip install .
```


### Notes

In case you get the error message *"Internal Error: printf is broken"* you can use the following workaround:

```bash
export LC_ALL=C
python script.py
```

This is a known gfortran bug, see the bottom of [http://projects.scipy.org/scipy/ticket/696]() for an explanation.
