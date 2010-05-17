# -*- coding: utf-8 -*-
"""
Python Bindings for multitaper `mtspec` f90 Library.
"""

import ctypes as C
from headers import mtspeclib
import numpy as np

def mtspec(data, delta, time_bandwidth, number_of_tapers = None, 
           quadratic = False, adaptive = True, verbose = False,
           optional_output = False, statistics = False, rshape = False,
           fcrit = False):
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
    (multidimensional) arrays containing the jackknife 95% confidence interval,
    the F statistics for single line and the number of degrees of freedom for
    each frequency bin.
    If both optional_output and statistics are true, the optional_outputs will
    be returned before the statistics.

    Returns
    -------
    list
        Returns a list with :class:`numpy.ndarray`. See the notes in the
        docstring for details.
    """

    # Use the optimal number of tapers in case no number is specified.
    if number_of_tapers is None:
        number_of_tapers = int(2*time_bandwidth) - 1

    # Transform the data to work with the library.
    data = np.require(data, 'float64', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])
    data_p = data.ctypes.data_as(C.POINTER(C.c_double))

    # Get some information necessary for the call to the Fortran library.
    npts = len(data)
    number_of_frequency_bins = int(npts/2) + 1

    # Create output arrays.
    spectrum = np.empty(number_of_frequency_bins,
                        'float64', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])
    spectrum_p = spectrum.ctypes.data_as(C.POINTER(C.c_double))
    frequency_bins = np.empty(number_of_frequency_bins,
                        'float64', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])
    frequency_bins_p = frequency_bins.ctypes.data_as(C.POINTER(C.c_double))

    # Create optional outputs.
    if optional_output is True:
        eigenspectra = np.empty((number_of_frequency_bins, number_of_tapers),
                        'float64', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])
        eigenspectra_p = eigenspectra.ctypes.data_as(C.POINTER(C.c_double))
        eigencoefficients = np.empty((npts, number_of_tapers),
                        'float64', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])
        eigencoefficients_p = eigencoefficients.ctypes.data_as(C.POINTER(C.c_double))
        weights = np.empty((number_of_frequency_bins,number_of_tapers),
                        'float64', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])
        weights_p = weights.ctypes.data_as(C.POINTER(C.c_double))
    else:
        eigenspectra_p = eigencoefficients_p = weights_p = None
    # Create statistics.
    if statistics is True:
        jackknife_interval = np.empty((number_of_frequency_bins,2),
                        'float64', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])
        jackknife_interval_p = jackknife_interval.ctypes.data_as(C.POINTER(C.c_double))
        f_statistics = np.empty(number_of_frequency_bins,
                        'float64', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])
        f_statistics_p = f_statistics.ctypes.data_as(C.POINTER(C.c_double))
        degrees_of_freedom = np.empty(number_of_frequency_bins,
                        'float64', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])
        degrees_of_freedom_p = degrees_of_freedom.ctypes.data_as(C.POINTER(C.c_double))
    else:
        jackknife_interval_p = f_statistics_p = degrees_of_freedom_p = None

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
    mtspeclib.mtspec_d_(C.byref(C.c_int(npts)),
                C.byref(C.c_double(delta)), data_p,
                C.byref(C.c_double(time_bandwidth)),
                C.byref(C.c_int(number_of_tapers)),
                C.byref(C.c_int(number_of_frequency_bins)),
                frequency_bins_p, spectrum_p, verbose, quadratic, adaptive,
                eigencoefficients_p, weights_p, jackknife_interval_p,
                degrees_of_freedom_p, eigenspectra_p, rshape, f_statistics_p,
                fcrit)

    # Figure out what to return. See the docstring of this method for details.
    return_values = [spectrum, frequency_bins]
    if optional_output is True:
        return_values.extend([eigenspectra, eigencoefficients, weights])
    if statistics is True:
        return_values.extend([jackknife_interval, f_statistics,
                              degrees_of_freedom])
    return return_values

def sine_mtspec(data, delta, number_of_tapers = None,
                number_of_iterations = 2, degree_of_smoothing = 1.0):
    if number_of_tapers is None:
        number_of_tapers = 0

    # Transform the data to work with the library.
    data = np.require(data, 'float32', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])

    npts = len(data)
    number_of_frequency_bins = int(npts/2) + 1

    # Create output arrays.
    frequency_bins = np.empty(number_of_frequency_bins)
    frequency_bins = np.require(frequency_bins, 'float32', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])
    spectrum = np.empty(number_of_frequency_bins)
    spectrum = np.require(spectrum, 'float32', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])

    number_of_tapers_per_freq_points = np.empty(number_of_frequency_bins)
    number_of_tapers_per_freq_points = np.require(number_of_tapers_per_freq_points,
                                                  'int32', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])

    errors = np.empty((number_of_frequency_bins,2))
    errors = np.require(errors, 'float32', ['F_CONTIGUOUS', 'ALIGNED', 'WRITEABLE'])
    # Call the library.
    mtspeclib.sine_psd_(C.byref(C.c_int(npts)),
                  C.byref(C.c_float(delta)),
                  data.ctypes.data_as(C.POINTER(C.c_double)), 
                  C.byref(C.c_int(number_of_tapers)),
                  C.byref(C.c_int(number_of_iterations)),
                  C.byref(C.c_float(degree_of_smoothing)),
                  C.byref(C.c_int(number_of_frequency_bins)),
                  frequency_bins.ctypes.data_as(C.POINTER(C.c_double)), 
                  spectrum.ctypes.data_as(C.POINTER(C.c_double)),
                  number_of_tapers_per_freq_points.ctypes.data_as(C.POINTER(C.c_double)),
                  errors.ctypes.data_as(C.POINTER(C.c_double)))

    return spectrum, frequency_bins
