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
from headers import mtspeclib
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

    Parameters
    ----------
    data : :class:`numpy.ndarray'
        Array with the data.
    delta : float
        Sample spacing of the data.
    time_bandwidth : float
        Time-bandwidth product. Common values are 2, 3, 4 and numbers in
        between.
    nfft : int
        Number of points for fft. If nfft == None, no zero padding
        will be applied before the fft
    number_of_tapers : integer, optional
        Number of tapers to use. Defaults to int(2*time_bandwidth) - 1. This is
        maximum senseful amount. More tapers will have no great influence on
        the final spectrum but increase the calculation time. Use fewer tapers
        for a faster calculation.
    quadratic : bool, optional
        Whether or not to caluclate a quadratic multitaper. Defaults to False.
    adaptive : bool, optional
        Whether to use adaptive or constant weighting of the eigenspectra.
        Defaults to True(adaptive).
    verbose : bool, optional
        Passed to the fortran library. Defaults to False.
    optional_output : bool, optional
        Calculates and returns additional output parameters. See the notes in
        the docstring for further details.
    statistics : bool, optional
        Calculates and returns statistics. See the notes in the docstring for
        further details.
    rshape : integer/None, optional
        Determines whether or not to perform the F-test for lines. If rshape is
        1 or 2, then don't put the lines back. If rshape is 2 only check around
        60 Hz. See the fortran source code for more informations. Defaults to
        None (do not perform the F-test).
    fcrit : float/None, optional
       The threshold probability for the F-test. If none is given, the mtspec
       library calculates a default value. See the fortran source code for
       details. Defaults to None.

    Notes
    -----
    This method will at return at least two arrays: The calculated spectrum and
    the corresponding frequencies.
    If optional_output is true it will also return (in the given order)
    (multidimensional) arrays containing the eigenspectra, the corresponding
    eigencoefficients and an array containing the weights for each eigenspectra
    normalized so that the sum of squares over the eigenspectra is one. 
    If statistics is True is will also return (in the given order)
    (multidimensional) arrays containing the jackknife 5% and 95% confidence
    intervals, the F statistics for single line and the number of degrees of
    freedom for each frequency bin.
    If both optional_output and statistics are true, the optional_outputs will
    be returned before the statistics.

    Returns
    -------
    list
        Returns a list with :class:`numpy.ndarray`. See the notes section
        in the docstring for details.
    """
    npts = len(data)

    # Depending if nfft is specified or not initialte MtspecTytpe
    # for mtspec_pad_ or mtspec_d_
    if nfft is None:
        nfft = npts
        mt = _MtspecType("float64") #mtspec_d_
    else:
        mt = _MtspecType("float32") #mtspec_pad_
    # Use the optimal number of tapers in case no number is specified.
    if number_of_tapers is None:
        number_of_tapers = int(2*time_bandwidth) - 1
    # Transform the data to work with the library.
    data = np.require(data, mt.float, mt.required)
    # Get some information necessary for the call to the Fortran library.
    number_of_frequency_bins = int(nfft/2) + 1
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
        jackknife_interval = mt.empty((number_of_frequency_bins,2))
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

    Parameters
    ----------
    data : :class:`numpy.ndarray'
        Array with the data.
    delta : float
        Sample spacing of the data.
    number_of_tapers : integer/None, optional
        Number of tapers to use. If none is given, the library will perform an
        adaptive taper estimation with a varying number of tapers for each
        frequency. Defaults to None.
    number_of_iterations : integer, optional
        Number of iterations to perform. Values less than 2 will be set to 2.
        Defaults to 2.
    degree_of_smoothing : float, optional
        Degree of smoothing. Defaults to 1.0.
    statistics : bool, optional
        Calculates and returns statistics. See the notes in the docstring for
        further details.
    verbose : bool, optional
        Passed to the fortran library. Defaults to False.

    Notes
    -----
    This method will at return at least two arrays: The calculated spectrum and
    the corresponding frequencies.
    If statistics is True is will also return (in the given order)
    (multidimensional) arrays containing the 1-std errors (a simple dof
    estimate) and the number of tapers used for each frequency point.

    Returns
    -------
    list
        Returns a list with :class:`numpy.ndarray`. See the notes in the
        docstring for details.
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
    data = np.require(data, 'float32', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])
    # Some variables necessary to call the library.
    npts = len(data)
    number_of_frequency_bins = int(npts/2) + 1
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
        errors = mt.empty((number_of_frequency_bins,2))
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
