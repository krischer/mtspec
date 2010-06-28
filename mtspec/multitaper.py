# -*- coding: utf-8 -*-
#-------------------------------------------------------------------
# Filename: multitaper.py
#  Purpose: Python wrapper for multitaper `mtspec` f90 library of
#           German A. Prieto
#   Author: Moritz Beyreuther, Lion Krischer
#    Email: beyreuth@geophysik.uni-muenchen.de
#  License: GPLv2
#
# Copyright (C) 2008-2010 Moritz Beyreuther, Lion Krischer
#---------------------------------------------------------------------
"""
Python Wrapper for multitaper `mtspec` f90 library of German A. Prieto.
"""

import ctypes as C
from util import mtspeclib
import numpy as np


def mtspec(data, delta, time_bandwidth, nfft=None, number_of_tapers=None,
           quadratic=False, adaptive=True, verbose=False,
           optional_output=False, statistics=False, rshape=False,
           fcrit=False):
    """
    Wrapper method for the mtspec subroutine in the library by German A.
    Prieto.

    This method estimates the adaptive weighted multitaper spectrum, as in
    Thomson 1982.  This is done by estimating the DPSS (discrete prolate
    spheroidal sequences), multiplying each of the tapers with the data series,
    take the FFT, and using the adaptive scheme for a better estimation.

    :param data: :class:`numpy.ndarray`
         Array with the data.
    :param delta: float
         Sample spacing of the data.
    :param time_bandwidth: float
         Time-bandwidth product. Common values are 2, 3, 4 and numbers in
         between.
    :param nfft: int
         Number of points for fft. If nfft == None, no zero padding
         will be applied before the fft
    :param number_of_tapers: integer, optional
         Number of tapers to use. Defaults to int(2*time_bandwidth) - 1. This
         is maximum senseful amount. More tapers will have no great influence
         on the final spectrum but increase the calculation time. Use fewer
         tapers for a faster calculation.
    :param quadratic: bool, optional
         Whether or not to caluclate a quadratic multitaper. Defaults to False.
    :param adaptive: bool, optional
         Whether to use adaptive or constant weighting of the eigenspectra.
         Defaults to True(adaptive).
    :param verbose: bool, optional
         Passed to the fortran library. Defaults to False.
    :param optional_output: bool, optional
         Calculates and returns additional output parameters. See the notes in
         the docstring for further details.
    :param statistics: bool, optional
         Calculates and returns statistics. See the notes in the docstring for
         further details.
    :param rshape: integer/None, optional
         Determines whether or not to perform the F-test for lines. If rshape
         is 1 or 2, then don't put the lines back. If rshape is 2 only check
         around 60 Hz. See the fortran source code for more informations.
         Defaults to None (do not perform the F-test).
    :param fcrit: float/None, optional
         The threshold probability for the F-test. If none is given, the mtspec
         library calculates a default value. See the fortran source code for
         details. Defaults to None.
    :return: Returns a list with :class:`numpy.ndarray`. See the note
         below.

    .. note::

        This method will at return at least two arrays: The calculated spectrum
        and the corresponding frequencies.  If optional_output is true it will
        also return (in the given order) (multidimensional) arrays containing
        the eigenspectra, the corresponding eigencoefficients and an array
        containing the weights for each eigenspectra normalized so that the sum
        of squares over the eigenspectra is one.  If statistics is True is will
        also return (in the given order) (multidimensional) arrays containing
        the jackknife 5% and 95% confidence intervals, the F statistics for
        single line and the number of degrees of freedom for each frequency
        bin.  If both optional_output and statistics are true, the
        optional_outputs will be returned before the statistics.
    """
    npts = len(data)

    # Depending if nfft is specified or not initialte MtspecTytpe
    # for mtspec_pad_ or mtspec_d_
    if nfft is None:
        nfft = npts
        mt = _MtspecType("float64")  # mtspec_d_
    else:
        mt = _MtspecType("float32")  # mtspec_pad_
    # Use the optimal number of tapers in case no number is specified.
    if number_of_tapers is None:
        number_of_tapers = int(2 * time_bandwidth) - 1
    # Transform the data to work with the library.
    data = np.require(data, mt.float, mt.required)
    # Get some information necessary for the call to the Fortran library.
    number_of_frequency_bins = int(nfft / 2) + 1
    # Create output arrays.
    spectrum = mt.empty(number_of_frequency_bins)
    frequency_bins = mt.empty(number_of_frequency_bins)
    # Create optional outputs.
    if optional_output is True:
        eigenspectra = mt.empty((number_of_frequency_bins, number_of_tapers))
        eigencoefficients = mt.empty((nfft, number_of_tapers), complex=True)
        weights = mt.empty((number_of_frequency_bins, number_of_tapers))
    else:
        eigenspectra = eigencoefficients = weights = None
    # Create statistics.
    if statistics is True:
        jackknife_interval = mt.empty((number_of_frequency_bins, 2))
        f_statistics = mt.empty(number_of_frequency_bins)
        degrees_of_freedom = mt.empty(number_of_frequency_bins)
    else:
        jackknife_interval = f_statistics = degrees_of_freedom = None
    # Verbose mode on or off.
    if verbose is True:
        verbose = C.byref(C.c_char('y'))
    else:
        verbose = None
    # Determine whether or not to compute the quadratic multitaper.
    if quadratic is True:
        quadratic = C.byref(C.c_int(1))
    else:
        quadratic = None
    # Determine whether to use adaptive or constant weighting of the
    # eigenspectra.
    if adaptive is True:
        adaptive = None
    else:
        adaptive = C.byref(C.c_int(1))
    # Determines whether or not to perform the F-test for lines. If rshape is 1
    # or 2, then don't put the lines back. If rshape is 2 only check around 60
    # Hz. See the fortran source code for more informations.
    if type(rshape) == int:
        rshape = C.byref(C.c_int(rshape))
    else:
        rshape = None
    # The threshold probability for the F-test. If none is given, the mtspec
    # library calculates a default value. See the fortran source code for
    # details.
    if type(fcrit) == float:
        fcrit = C.byref(C.c_float(fcrit))
    else:
        fcrit = None
    # Call the library. Fortran passes pointers!
    args = [C.byref(C.c_int(npts)), C.byref(C.c_int(nfft)),
            C.byref(mt.c_float(delta)), mt.p(data),
            C.byref(mt.c_float(time_bandwidth)),
            C.byref(C.c_int(number_of_tapers)),
            C.byref(C.c_int(number_of_frequency_bins)), mt.p(frequency_bins),
            mt.p(spectrum), verbose, quadratic, adaptive,
            mt.p(eigencoefficients), mt.p(weights),
            mt.p(jackknife_interval), mt.p(degrees_of_freedom),
            mt.p(eigenspectra), rshape, mt.p(f_statistics), fcrit, None]
    # diffrent arguments, depending on mtspec_pad_ or mtspec_d_, adapt
    if npts == nfft:
        args.pop(1)

    # finally call the shared library function
    mt.mtspec(*args)

    # Figure out what to return. See the docstring of this method for details.
    return_values = [spectrum, frequency_bins]
    if optional_output is True:
        return_values.extend([eigenspectra, eigencoefficients, weights])
    if statistics is True:
        return_values.extend([jackknife_interval, f_statistics,
                              degrees_of_freedom])
    return return_values


def sine_psd(data, delta, number_of_tapers=None, number_of_iterations=2,
             degree_of_smoothing=1.0, statistics=False, verbose=False):
    """
    Wrapper method for the sine_psd subroutine in the library by German A.
    Prieto.

    The subroutine is in charge of estimating the adaptive sine multitaper as
    in Riedel and Sidorenko (1995).
    This is done by performing a MSE adaptive estimation. First a pilot
    spectral estimate is used, and S" is estimated, in order to get te number
    of tapers to use, using (13) of Riedel and Sidorenko for a min square error
    spectrum.
    Unlike the prolate spheroidal multitapers, the sine multitaper adaptive
    process introduces a variable resolution and error in the frequency domain.
    Complete error information is contained in the output variables as the
    corridor of 1-standard-deviation errors, and in the number of tapers used
    at each frequency.  The errors are estimated in the simplest way, from the
    number of degrees of freedom (two per taper), not by jack-knifing. The
    frequency resolution is found from K*fN/Nf where fN is the Nyquist
    frequency and Nf is the number of frequencies estimated.  The adaptive
    process used is as follows. A quadratic fit to the log PSD within an
    adaptively determined frequency band is used to find an estimate of the
    local second derivative of the spectrum. This is used in an equation like R
    & S (13) for the MSE taper number, with the difference that a parabolic
    weighting is applied with increasing taper order. Because the FFTs of the
    tapered series can be found by resampling the FFT of the original time
    series (doubled in length and padded with zeros) only one FFT is required
    per series, no matter how many tapers are used. This makes the program
    fast. Compared with the Thomson multitaper programs, this code is not only
    fast but simple and short. The spectra associated with the sine tapers are
    weighted before averaging with a parabolically varying weight. The
    expression for the optimal number of tapers given by R & S must be modified
    since it gives an unbounded result near points where S" vanishes, which
    happens at many points in most spectra. This program restricts the rate of
    growth of the number of tapers so that a neighboring covering interval
    estimate is never completely contained in the next such interval.

    This method SHOULD not be used for sharp cutoffs or deep valleys, or small
    sample sizes. Instead use Thomson multitaper in mtspec in this same
    library.

    :param data: :class:`numpy.ndarray`
        Array with the data.
    :param delta: float
        Sample spacing of the data.
    :param number_of_tapers: integer/None, optional
        Number of tapers to use. If none is given, the library will perform an
        adaptive taper estimation with a varying number of tapers for each
        frequency. Defaults to None.
    :param number_of_iterations: integer, optional
        Number of iterations to perform. Values less than 2 will be set to 2.
        Defaults to 2.
    :param degree_of_smoothing: float, optional
        Degree of smoothing. Defaults to 1.0.
    :param statistics: bool, optional
        Calculates and returns statistics. See the notes in the docstring for
        further details.
    :param verbose: bool, optional
        Passed to the fortran library. Defaults to False.
    :return: Returns a list with :class:`numpy.ndarray`. See the note below
        for details.

    .. note::

        This method will at return at least two arrays: The calculated
        spectrum and the corresponding frequencies.  If statistics is True
        is will also return (in the given order) (multidimensional) arrays
        containing the 1-std errors (a simple dof estimate) and the number
        of tapers used for each frequency point.
    """
    # Verbose mode on or off.
    if verbose is True:
        verbose = C.byref(C.c_char('y'))
    else:
        verbose = None
    # Set the number of tapers so it can be read by the library.
    if number_of_tapers is None:
        number_of_tapers = 0
    # initialize _MtspecType to save some space
    mt = _MtspecType("float32")
    # Transform the data to work with the library.
    data = np.require(data, mt.float, mt.required)
    # Some variables necessary to call the library.
    npts = len(data)
    number_of_frequency_bins = int(npts / 2) + 1
    # Create output arrays.
    frequency_bins = mt.empty(number_of_frequency_bins)
    spectrum = mt.empty(number_of_frequency_bins)
    # Create optional arrays or set to None.
    if statistics is True:
        # here an exception, mt sets the type float32, here we need int32
        # that is do all the type and POINTER definition once by hand
        tapers_per_freq_point = np.empty(number_of_frequency_bins,
                                         'int32', mt.required)
        tapers_per_freq_point_p = \
                tapers_per_freq_point.ctypes.data_as(C.POINTER(C.c_int))
        errors = mt.empty((number_of_frequency_bins, 2))
    else:
        tapers_per_freq_point_p = errors = None
    # Call the library. Fortran passes pointers!
    mtspeclib.sine_psd_(C.byref(C.c_int(npts)),
                  C.byref(C.c_float(delta)), mt.p(data),
                  C.byref(C.c_int(number_of_tapers)),
                  C.byref(C.c_int(number_of_iterations)),
                  C.byref(C.c_float(degree_of_smoothing)),
                  C.byref(C.c_int(number_of_frequency_bins)),
                  mt.p(frequency_bins), mt.p(spectrum),
                  tapers_per_freq_point_p, mt.p(errors), verbose)
    # Calculate return values.
    return_values = [spectrum, frequency_bins]
    if statistics is True:
        return_values.extend([errors, tapers_per_freq_point])
    return return_values


def dpss(npts, fw, nev, auto_spline=True, nmax=None):
    """
    Wrapper method for the dpss subroutine in the library by German A.
    Prieto.

    Calculation of the Discrete Prolate Spheroidal Sequences also knows as the
    slepian sequences, and the correspondent eigenvalues. Also, the (1 -
    eigenvalue) terms are calculated.

    By default this routine will use spline interpolation if sequences with
    more than 200000 samples are requested.

    :param npts: The number of points in the series
    :param fw: the time-bandwidth product (number of Rayleigh bins)
    :param nev: the desired number of tapers
    :param auto_spline: Whether or not to automatically use spline
        interpolation with npts > 200000. Defaults to True.
    :param nmax: The number of actual points to calculate the dpss. If this
        number is smaller than npts spline interpolation will be performed,
        regardless of auto_spline.
    :return: (v, lamb, theta) with v(npts,nev) the eigenvectors (tapers)
        lamb the eigenvalues of the v's and theta the 1-lambda (energy
        outside the bandwidth) values.

    .. note::

        The tapers are the eigenvectors of the tridiagonal matrix sigma(i,j)
        [see Slepian(1978) eq 14 and 25.] They are also the eigenvectors of
        the Toeplitz matrix eq. 18. We solve the tridiagonal system in
        tridib and tinvit for the tapers and use them in the integral
        equation in the frequency domain (dpss_ev subroutine) to get the
        eigenvalues more accurately, by performing Chebychev Gaussian
        Quadrature following Thomson's codes.
    """
    mt = _MtspecType("float64")

    v = mt.empty((npts, nev))
    lamb = mt.empty(nev)
    theta = mt.empty(nev)

    # Set auto_spline to True.
    if nmax and nmax < npts:
        auto_spline = True
    # Always set nmax.
    else:
        nmax = 200000

    # Call either the spline routine or the normal routine.
    if auto_spline is True and npts > nmax:
        mtspeclib.dpss_spline_(C.byref(C.c_int(nmax)), C.byref(C.c_int(npts)),
                               C.byref(C.c_double(fw)), C.byref(C.c_int(nev)),
                               mt.p(v), mt.p(lamb), mt.p(theta))
    else:
        mtspeclib.dpss_(C.byref(C.c_int(npts)), C.byref(C.c_double(fw)),
                        C.byref(C.c_int(nev)), mt.p(v), mt.p(lamb),
                        mt.p(theta))

    return (v, lamb, theta)


def wigner_ville_spectrum(data, delta, time_bandwidth=3.5,
                          number_of_tapers=None, smoothing_filter=None, 
                          filter_width=100, frac=1, verbose=False):
    """
    Wrapper method of the modified wv_spec (wv_spec_to_array) subroutine in
    the library of German A. Prieto.

    It is very slow for large arrays so try with a small one (< 5000 samples)
    first, or adapt frac respectively.

    :param data: numpy.ndarray;
        The input signal
    :param data: integer;
        The input sampling interval
    :param time_bandwidth: float;
        Time bandwith product
    :param number_of_tapers: int;
        Number of tapers to use. If None the number will be automatically
        determined
    :param smoothing_filter: string;
        On of 'boxcar', 'gauss' or just None
    :param filter_width: int;
        Filter width in samples
    :param frac: int;
        Fraction of output frequencies. E.g. 1 means output every frequency
        point, 3 means output every second frequency point
    :param verbose: bool;
        If True turn on verbose output
    """
    data = np.require(data, 'float32')
    mt = _MtspecType("float32")

    npts = len(data)

    # Use the optimal number of tapers in case no number is specified.
    if number_of_tapers is None:
        number_of_tapers = int(2 * time_bandwidth) - 1

    # Determine filter.
    if not smoothing_filter:
        smoothing_filter = 0
    elif smoothing_filter == 'boxcar':
        smoothing_filter = 1
    elif smoothing_filter == 'gauss':
        smoothing_filter = 2
    else:
        msg = 'Invalid value for smoothing filter.'
        raise Exception(msg)

    # Verbose mode on or off.
    if verbose:
        verbose = C.byref(C.c_char('y'))
    else:
        verbose = None

    # Allocate the output array
    output = mt.empty((npts//frac, npts), complex=True)

    mtspeclib.wv_spec_to_array_(C.byref(C.c_int(npts)),
                                C.byref(C.c_float(delta)), 
                                mt.p(data), mt.p(output), 
                                C.byref(C.c_int(frac)),
                                C.byref(C.c_float(time_bandwidth)),
                                C.byref(C.c_int(number_of_tapers)),
                                C.byref(C.c_int(smoothing_filter)),
                                C.byref(C.c_float(filter_width)), verbose)

    return output


class _MtspecType(object):
    """
    Simple class that stores type definition for interfacing with
    the fortran code and provides some helper functions.
    """
    struct = {"float32": (C.c_float, mtspeclib.mtspec_pad_),
              "float64": (C.c_double, mtspeclib.mtspec_d_)}

    def __init__(self, dtype):
        """
        Depending on dtype initialize different type structures.

        :param dtype: 'float32' or 'float64'
        """
        if dtype not in self.struct.keys():
            raise ValueError("dtype must be either 'float32' or 'float64'")
        self.float = dtype
        self.complex = 'complex%d' % (2 * float(dtype[-2:]))
        self.c_float = self.struct[dtype][0]
        self.pointer = C.POINTER(self.c_float)
        self.required = ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE']
        self.mtspec = self.struct[dtype][1]

    def empty(self, shape, complex=False):
        """
        A wrapper around np.empty which automatically sets the correct type
        and returns an empty array.

        :param shape: The shape of the array in np.empty format
        """
        if complex:
            return np.empty(shape, self.complex, self.required)
        return np.empty(shape, self.float, self.required)

    def p(self, ndarray):
        """
        A wrapper around ctypes.data_as which automatically sets the
        correct type. Returns none if ndarray is None.

        :param ndarray: numpy input array or None
        """
        # short variable name for passing as argument in function calls
        if ndarray is None:
            return None
        return ndarray.ctypes.data_as(self.pointer)
