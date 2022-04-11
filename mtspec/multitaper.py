#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Main functions of mtspec.

:copyright:
    Lion Krischer (krischer@geophysik.uni-muenchen.de) and
    Moritz Beyreuther, 2010-2016
:license:
    GNU General Public License, Version 3
    (http://www.gnu.org/copyleft/gpl.html)
"""
import ctypes as C
import numpy as np

from .util import _load_lib


mtspeclib = _load_lib()


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
    take the FFT, and using the adaptive scheme for a better estimation. It
    outputs the power spectral density (PSD).

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
         Whether or not to caluclate a quadratic multitaper. Will only work
         if nfft is False or equal to the sample count. The nfft parameter
         will overwrite the quadratic paramter. Defaults to False.
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
        of squares over the eigenspectra is one.  If statistics is True it will
        also return (in the given order) (multidimensional) arrays containing
        the jackknife 5% and 95% confidence intervals, the F statistics for
        single line and the number of degrees of freedom for each frequency
        bin.  If both optional_output and statistics are true, the
        optional_outputs will be returned before the statistics.
    """
    npts = len(data)

    # Depending if nfft is specified or not initialte MtspecTytpe
    # for mtspec_pad_ or mtspec_d_
    complex = any(np.iscomplex(data))
    if nfft is None or nfft == npts:
        nfft = npts
        mt = _MtspecType("float64")  # mtspec_d_
    else:
        mt = _MtspecType("float32")  # mtspec_pad_
        quadratic = False
    if complex:
        mt = _MtspecType("float32")  # mtspec_c_
        mt.mtspec = mtspeclib.mtspec_c_
    # Use the optimal number of tapers in case no number is specified.
    if number_of_tapers is None:
        number_of_tapers = int(2 * time_bandwidth) - 1
    # Transform the data to work with the library.
    if complex:
        data = np.require(data, dtype=mt.complex, requirements=[mt.order])
    else:
        data = np.require(data, dtype=mt.float, requirements=[mt.order])
    # Get some information necessary for the call to the Fortran library.
    if complex:
        number_of_frequency_bins = nfft
    else:
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
            C.byref(mt.c_float(delta)), mt.p(data, complex),
            C.byref(mt.c_float(time_bandwidth)),
            C.byref(C.c_int(number_of_tapers)),
            C.byref(C.c_int(number_of_frequency_bins)), mt.p(frequency_bins),
            mt.p(spectrum), verbose, quadratic, adaptive,
            mt.p(eigencoefficients), mt.p(weights),
            mt.p(jackknife_interval), mt.p(degrees_of_freedom),
            mt.p(eigenspectra), rshape, mt.p(f_statistics), fcrit, None]
    # diffrent arguments, depending on mtspec_pad_ or mtspec_d_, adapt
    if npts == nfft:
        #print('npts == nfft')
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
    in Riedel and Sidorenko (1995). It outputs the power spectral density
    (PSD).

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
    data = np.require(data, dtype=mt.float, requirements=[mt.order])
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
                                         dtype='int32', order=mt.order)
        tapers_per_freq_point_p = \
            tapers_per_freq_point.ctypes.data_as(C.POINTER(C.c_int))
        errors = mt.empty((number_of_frequency_bins, 2))
    else:
        tapers_per_freq_point_p = errors = None
    # Call the library. Fortran passes pointers!
    mtspeclib.sine_psd_(
        C.byref(C.c_int(npts)),
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


def dpss(npts, fw, number_of_tapers, auto_spline=True, npts_max=None):
    """
    Calculates DPSS also known as Slepian sequences or Slepian tapers.

    Calculation of the DPSS (Discrete Prolate Spheroidal Sequences) and the
    correspondent eigenvalues. The (1 - eigenvalue) terms are also calculated.

    Wraps the ``dpss()`` subroutine from the Fortran library.

    By default this routine will use spline interpolation if sequences with
    more than 200.000 samples are requested.

    .. note::

        The tapers are the eigenvectors of the tridiagonal matrix sigma(i, j)
        [see Slepian(1978) eq 14 and 25]. They are also the eigenvectors of
        the Toeplitz matrix, eq. 18.

    :param npts: The number of points in the series.
    :type npts: int
    :param fw: The time-bandwidth product (number of Rayleigh bins).
    :type fw: float
    :param number_of_tapers: The desired number of tapers.
    :type number_of_tapers: int
    :param auto_spline: Whether or not to automatically use spline
        interpolation for ``npts`` > 200000.
    :type auto_spline: bool
    :param npts_max: The number of actual points to calculate the DPSS. If this
        number is smaller than ``npts``, spline interpolation will be
        performed, regardless of the value of ``auto_spline``.
    :type npts_max: None or int
    :returns: ``(v, lambda, theta)`` with
        ``v(npts, number_of_tapers)`` the eigenvectors (tapers), ``lambda`` the
        eigenvalues of the ``v``'s and ``theta`` the 1 - ``lambda``
        (energy outside the bandwidth) values.

    .. rubric:: Example

    This example demonstrates how to calculate and plot the first five DPSS'.

    >>> import matplotlib.pyplot as plt
    >>> from mtspec import dpss
    >>> tapers, lamb, theta = dpss(512, 2.5, 5)
    >>> for i in range(5):
    ...     plt.plot(tapers[:, i])

    .. plot ::

        # Same as the code snippet in the docstring, just a bit prettier.
        import matplotlib.pyplot as plt
        plt.style.use("ggplot")
        from mtspec import dpss
        tapers, lamb, theta = dpss(512, 2.5, 5)
        for i in range(5):
            plt.plot(tapers[:, i])
        plt.xlim(0, 512)
        plt.ylim(-0.09, 0.09)
        plt.tight_layout()
    """
    mt = _MtspecType("float64")

    v = mt.empty((npts, number_of_tapers))
    lamb = mt.empty(number_of_tapers)
    theta = mt.empty(number_of_tapers)

    # Set auto_spline to True.
    if npts_max and npts_max < npts:
        auto_spline = True
    # Always set npts_max.
    else:
        npts_max = 200000

    # Call either the spline routine or the normal routine.
    if auto_spline is True and npts > npts_max:
        mtspeclib.dpss_spline_(
            C.byref(C.c_int(npts_max)), C.byref(C.c_int(npts)),
            C.byref(C.c_double(fw)), C.byref(C.c_int(number_of_tapers)),
            mt.p(v), mt.p(lamb), mt.p(theta))
    else:
        mtspeclib.dpss_(C.byref(C.c_int(npts)), C.byref(C.c_double(fw)),
                        C.byref(C.c_int(number_of_tapers)), mt.p(v),
                        mt.p(lamb), mt.p(theta))

    return (v, lamb, theta)


def wigner_ville_spectrum(data, delta, time_bandwidth=3.5,
                          number_of_tapers=None, smoothing_filter=None,
                          filter_width=100, frequency_divider=1,
                          verbose=False):
    """
    Function to calculate the Wigner-Ville Distribution or Wigner-Ville
    Spectrum of a signal using multitaper spectral estimates.

    In general it gives better temporal and frequency resolution than a
    spectrogram but introduces many artifacts and possibly negative values
    which are not physical. This can be alleviated a bit by applying a
    smoothing kernel which is also known as a reduced interference
    distribution (RID).

    Wraps the ``wv_spec()`` and ``wv_spec_to_array()`` subroutines of the
    Fortran library.

    It is very slow for large arrays so try with a small one (< 5000 samples)
    first.

    :param data: The input signal.
    :type data: numpy.ndarray
    :param delta: The sampling interval of the data.
    :type delta: float
    :param time_bandwidth: Time bandwidth product for the tapers.
    :type time_bandwidth: float
    :param number_of_tapers: Number of tapers to use. If ``None``, the number
        will be automatically determined from the time bandwidth product
        which is usually the optimal choice.
    :type number_of_tapers: int
    :param smoothing_filter: One of ``"boxcar"``, ``"gauss"`` or just ``None``
    :type smoothing_filter: str
    :param filter_width: Filter width in samples.
    :type filter_width: int
    :param frequency_divider: This method will always calculate all
        frequencies from 0 ... Nyquist frequency. This parameter allows the
        adjustment of the maximum frequency, so that the frequencies range
        from 0 .. Nyquist frequency / int(frequency_divider).
    :type frequency_divider: int
    :param verbose: Verbose output on/off.
    :type verbose: bool

    .. rubric:: Example

    This example demonstrates how to plot a signal, its multitaper spectral
    estimate, and its Wigner-Ville time-frequency distribution. The signal
    is sinusoidal overlaid with two simple linear chirps.

    >>> import matplotlib.pyplot as plt
    >>> import numpy as np
    >>> from mtspec import mtspec, wigner_ville_spectrum
    >>> from mtspec.util import signal_bursts
    >>> fig = plt.figure()

    Get the example signal.

    >>> data = signal_bursts()

    Plot the data on the top axes.

    >>> ax1 = fig.add_axes([0.2,0.75, 0.79, 0.23])
    >>> ax1.plot(data, color="0.1")
    >>> ax1.set_xlim(0, len(data))

    Plot its spectral estimate on the side.

    >>> ax2 = fig.add_axes([0.06,0.02,0.13,0.69])
    >>> spec, freq = mtspec(data, 10, 3.5)
    >>> ax2.plot(spec, freq, color="0.1")
    >>> ax2.set_xlim(0, spec.max())
    >>> ax2.set_ylim(freq[0], freq[-1])
    >>> ax2.set_xticks([])

    Create and plot the Wigner-Ville distribution.

    >>> wv = wigner_ville_spectrum(data, 10, 3.5,
    ...                            smoothing_filter='gauss')
    >>> ax3 = fig.add_axes([0.2, 0.02, 0.79, 0.69])
    >>> ax3.set_yticks([])
    >>> ax3.set_xticks([])
    >>> # The square root only serves plotting purposes.
    >>> ax3.imshow(np.sqrt(abs(wv)), interpolation='lanczos',
    ...            aspect='auto', cmap="magma")

    .. plot::

        # Same as the above code snippet just a bit prettier.
        import matplotlib.pyplot as plt
        plt.style.use("ggplot")
        from mtspec import mtspec, wigner_ville_spectrum
        from mtspec.util import signal_bursts
        import numpy as np

        fig = plt.figure()
        data = signal_bursts()

        # Plot the data
        ax1 = fig.add_axes([0.2,0.75, 0.79, 0.23])
        ax1.plot(data, color="0.1")
        ax1.set_xlim(0, len(data))

        # Plot multitaper spectrum
        ax2 = fig.add_axes([0.06,0.02,0.13,0.69])
        spec, freq = mtspec(data, 10, 3.5)
        ax2.plot(spec, freq, color="0.1")
        ax2.set_xlim(0, spec.max())
        ax2.set_ylim(freq[0], freq[-1])
        ax2.set_xticks([])

        # Create the wigner ville spectrum
        wv = wigner_ville_spectrum(data, 10, 3.5, smoothing_filter='gauss')

        # Plot the WV
        ax3 = fig.add_axes([0.2, 0.02, 0.79, 0.69])
        ax3.set_yticks([])
        ax3.set_xticks([])
        ax3.imshow(np.sqrt(abs(wv)), interpolation='lanczos', aspect='auto',
                   cmap="magma")
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
    # f90 code internally pads zeros to 2 * npts. That is we only return
    # every second frequency point, thus decrease the size of the array
    output = mt.empty((npts // 2 // int(frequency_divider) + 1, npts))

    mtspeclib.wv_spec_to_array_(C.byref(C.c_int(npts)),
                                C.byref(C.c_float(delta)),
                                mt.p(data), mt.p(output),
                                C.byref(C.c_float(time_bandwidth)),
                                C.byref(C.c_int(number_of_tapers)),
                                C.byref(C.c_int(smoothing_filter)),
                                C.byref(C.c_float(filter_width)),
                                C.byref(C.c_int(frequency_divider)), verbose)

    return output


def mt_coherence(df, xi, xj, tbp, kspec, nf, p, **kwargs):
    """
    Construct the coherence spectrum from the yk's and the
    weights of the usual multitaper spectrum estimation.
    Note this code uses the real(4) multitaper code.

    INPUT

    :param df: float; sampling rate of time series
    :param xi: numpy.ndarray; data for first series
    :param xj: numpy.ndarray; data for second series
    :param tbp: float; the time-bandwidth product
    :param kspec: integer; number of tapers to use
    :param nf: integer; number of freq points in spectrum
    :param p:  float; confidence for null hypothesis test, e.g. .95


    OPTIONAL INPUT

    :param iadapt:  integer 0 - adaptive, 1 - constant weights
             default adapt = 1

    OPTIONAL OUTPUTS, the outputs are returned as dictionary, with keys as
    specified below and values as numpy.ndarrays. In order to activate the
    output set the corresponding kwarg in the argument list, e.g.
    ``mt_coherence(df, xi, xj, tbp, kspec, nf, p, freq=True, cohe=True)``

    :param freq:     the frequency bins
    :param cohe:     coherence of the two series (0 - 1)
    :param phase:    the phase at each frequency
    :param speci:    spectrum of first series
    :param specj:    spectrum of second series
    :param conf:     p confidence value for each freq.
    :param cohe_ci:  95% bounds on coherence (not larger than 1)
    :param phase_ci: 95% bounds on phase estimates

    If confidence intervals are requested, then both phase and
    cohe variables need to be requested as well.
    """
    npts = len(xi)
    if len(xj) != npts:
        raise Exception("Inpurt ndarrays have mismatching length")
    mt = _MtspecType('float32')

    # convert type of input arguments if necessary
    xi = np.require(xi, dtype=mt.float, requirements=[mt.order])
    xj = np.require(xj, dtype=mt.float, requirements=[mt.order])

    # fill up optional arguments, if not given set them None
    args = []
    for key in ('freq', 'cohe', 'phase', 'speci', 'specj', 'conf',
                'cohe_ci', 'phase_ci', 'iadapt'):
        kwargs.setdefault(key, None)
        if key in ('cohe_ci', 'phase_ci') and kwargs[key]:
            kwargs[key] = mt.empty(nf, 2)
            args.append(mt.p(kwargs[key]))
        elif key == 'iadapt' and kwargs[key]:
            args.append(C.byref(C.c_int(kwargs[key])))
        elif kwargs[key]:
            kwargs[key] = mt.empty(nf)
            args.append(mt.p(kwargs[key]))
        else:
            args.append(kwargs[key])

    mtspeclib.mt_cohe_(C.byref(C.c_int(int(npts))),
                       C.byref(C.c_float(float(df))),
                       mt.p(xi), mt.p(xj), C.byref(C.c_float(float(tbp))),
                       C.byref(C.c_int(int(kspec))), C.byref(C.c_int(int(nf))),
                       C.byref(C.c_float(float(p))), *args)

    # remove None values from dictionary
    return dict([(k, v) for k, v in kwargs.items() if v is not None])


def mt_deconvolve(data_a, data_b, delta, nfft=None, time_bandwidth=None,
                  number_of_tapers=None, weights="adaptive", demean=True,
                  fmax=0.0):
    """
    Deconvolve two time series using multitapers.

    This uses the eigencoefficients and the weights from the multitaper
    spectral estimations and more or less follows this paper:

    .. |br| raw:: html

        <br />

    **Receiver Functions from Multiple-Taper Spectral Correlation Estimates**
    *Jeffrey Park, Vadim Levin* |br|
    Bulletin of the Seismological Society of America Dec 2000,
    90 (6) 1507-1520
    http://dx.doi.org/10.1785/0119990122

    :type data_a: :class:`numpy.ndarray`
    :param data_a: Data for first time series.
    :type data_b: :class:`numpy.ndarray`
    :param data_b: Data for second time series.
    :type delta: float
    :param delta: Sample spacing of the data.
    :type nfft: int
    :param nfft: Number of points for the FFT. If ``nfft == None``, no zero
        padding will be applied before the FFT.
    :type time_bandwidth: float
    :param time_bandwidth: Time-bandwidth product. Common values are 2, 3, 4,
        and numbers in between.
    :type number_of_tapers: int
    :param number_of_tapers: Number of tapers to use. Defaults to
        ``int(2*time_bandwidth) - 1``. This is maximum senseful amount. More
        tapers will have no great influence on the final spectrum but increase
        the calculation time. Use fewer tapers for a faster calculation.
    :type weights: str
    :param weights: ``"adaptive"`` or ``"constant"`` weights.
    :type deman: bool
    :param demean: Force the complex TF to be demeaned.
    :type fmax: float
    :param fmax: Maximum frequency for lowpass cosine filter. Set this to
        zero to not have a filter.

    :return: Returns a dictionary with 5 :class:`numpy.ndarray`'s. See the note
        below.

    .. note::

        Returns a dictionary with five arrays:

        * ``"deconvolved"``: Deconvolved time series.
        * ``"spectrum_a"``: Spectrum of the first time series.
        * ``"spectrum_b"``:  Spectrum of the second time series.
        * ``"spectral_ratio"``: The ratio of both spectra.
        * ``"frequencies"``: The used frequency bins for the spectra.

    """
    npts = len(data_a)
    if len(data_b) != npts:
        raise ValueError("Input arrays must have the same length!")

    if nfft is None:
        nfft = npts
    elif nfft < npts:
        raise ValueError("nfft must be larger then the number of samples in "
                         "the array.")

    # Deconvolution utilizes the 32bit version.
    mt = _MtspecType("float32")

    # Use the optimal number of tapers in case no number is specified.
    if number_of_tapers is None:
        number_of_tapers = int(2 * time_bandwidth) - 1

    # Transform the data to work with the library.
    data_a = np.require(data_a, mt.float, requirements=[mt.order])
    data_b = np.require(data_b, mt.float, requirements=[mt.order])

    nf = nfft // 2 + 1

    # Internally uses integers
    if demean:
        demean = 1
    else:
        demean = 0

    # iad = 0 are adaptive, iad = 1 are constant weight - this is
    # counter intuitive.
    if weights == "constant":
        adaptive = 1
    elif weights == "adaptive":
        adaptive = 0
    else:
        raise ValueError('Weights must be either "adaptive" or "constant".')

    tfun = mt.empty(nfft)
    freq = mt.empty(nf)
    spec_ratio = mt.empty(nf)
    speci = mt.empty(nf)
    specj = mt.empty(nf)

    mtspeclib.mt_deconv_(
        C.byref(C.c_int(int(npts))),
        C.byref(C.c_int(int(nfft))),
        C.byref(C.c_float(float(delta))),
        mt.p(data_a),
        mt.p(data_b),
        C.byref(C.c_float(float(time_bandwidth))),
        C.byref(C.c_int(int(number_of_tapers))),
        C.byref(C.c_int(int(nf))),
        C.byref(C.c_int(adaptive)),
        mt.p(freq),
        mt.p(tfun),
        mt.p(spec_ratio),
        mt.p(speci),
        mt.p(specj),
        C.byref(C.c_int(demean)),
        C.byref(C.c_float(fmax)))

    return {
        "frequencies": freq,
        "deconvolved": tfun,
        "spectral_ratio": spec_ratio,
        "spectrum_a": speci,
        "spectrum_b": specj
    }


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
            raise ValueError("dtype must be either 'float32' or 'float64'"
                             + " or 'complex32'")
        self.float = dtype
        #self.real = 'float%d' % (float(dtype[-2:]))
        self.complex = 'complex%d' % (2*float(dtype[-2:]))
        #self.complex = 'complex%d' % (float(dtype[-2:]))
        # aboves leads to: TypeError: data type "complex32" not understood
        self.c_float = self.struct[dtype][0]
        self.pointer = C.POINTER(self.c_float)
        self.order = "F"
        self.mtspec = self.struct[dtype][1]

    def empty(self, shape, complex=False):
        """
        A wrapper around np.empty which automatically sets the correct type
        and returns an empty array.

        :param shape: The shape of the array in np.empty format
        """
        if complex:
            return np.empty(shape, dtype=self.complex, order=self.order)
        return np.empty(shape, dtype=self.float, order=self.order)

    def p(self, ndarray, complex=False):
        """
        A wrapper around ctypes.data_as which automatically sets the
        correct type. Returns none if ndarray is None.

        :param ndarray: numpy input array or None
        """
        # short variable name for passing as argument in function calls
        if ndarray is None:
            return None
        if complex:
            pointer_complex = C.POINTER(2*self.c_float)
            return ndarray.ctypes.data_as(pointer_complex)
        else:
            return ndarray.ctypes.data_as(self.pointer)
