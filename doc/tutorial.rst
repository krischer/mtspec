Tutorial
========

This tutorial shows source code on how to recreate the first three figures from
the multitaper paper of Prieto. Simple examples for the DPSS method, the
Wigner Ville spectrum, and multitaper coherency calculations are included as
well. More information can be found in the Prieto paper (reference_).

.. _reference: http://dx.doi.org/10.1016/j.cageo.2008.06.007

.. contents::
    :local:


Fig. 1: Multitaper Spectrum Estimate with Confidence Intervals
--------------------------------------------------------------

This example shows data at the top and a multitaper spectrum estimate on the
bottom with 95% confidence intervals shaded in red.
See the documentation of the :func:`mtspec.multitaper.mtspec` function for
more details.

.. plot:: snippets/figure_1.py

.. literalinclude:: snippets/figure_1.py
   :language: python


Fig. 2: F Statistics
--------------------

The top shows the F statistics and the bottom a spectrum reshaped with these.
Function documentation: :func:`mtspec.multitaper.mtspec`.

.. plot:: snippets/figure_2.py

.. literalinclude:: snippets/figure_2.py
    :language: python


Fig. 3: Various Multitaper Variants
-----------------------------------

Comparison of different multitaper spectral estimates and various settings.
Top shows data, bottom 4 plots are spectral estimations. Function
documentations: :func:`mtspec.multitaper.mtspec` and
:func:`mtspec.multitaper.sine_psd`.

.. plot:: snippets/figure_3.py

.. literalinclude:: snippets/figure_3.py
    :language: python


DPSS Example
------------

A simple example showing how to calculate and plot a couple DPSS. Function
documentation: :func:`mtspec.multitaper.dpss`.

.. plot:: snippets/dpss_example.py

.. literalinclude:: snippets/dpss_example.py
    :language: python


Wigner-Ville Spectrum
---------------------

Time frequency like spectral estimation using the Wigner-Ville method.
Function documentation: :func:`mtspec.multitaper.wigner_ville_spectrum`.

.. plot:: snippets/wigner_ville_spectrum_example.py

.. literalinclude:: snippets/wigner_ville_spectrum_example.py
    :language: python


Wigner-Ville Smoothing
++++++++++++++++++++++

One of the main disadvantages of the Wigner-Ville method is occurrence of
interference terms. This can be somewhat alleviated by smoothing it at the
cost of the clarity of the distribution.

The following plot shows an example of this behaviour. The signal consists of
a linear chirp and an exponential chirp. The left figure is with smoothing and
the right one without it.

.. plot:: snippets/wigner_ville_smoothing.py

.. literalinclude:: snippets/wigner_ville_smoothing.py
    :language: python


Multitaper coherence example
----------------------------

Calculate the coherency of two signals using multitapers. Function
documentation: :func:`mtspec.multitaper.mt_coherence`.

.. plot:: snippets/multitaper_coherency_example.py

.. literalinclude:: snippets/multitaper_coherency_example.py
    :language: python


Recreate Fig. 6 - Deconvolution
-------------------------------
.. plot::

    import matplotlib as mpl
    mpl.rcParams['font.size'] = 9.0
    import matplotlib.pyplot as plt
    import numpy as np
    import scipy.fftpack

    from mtspec import mt_deconv, mtspec
    from mtspec.util import _load_mtdata

    sampling_rate = 1.0
    ngf = 500
    nf2 = ngf/2+1
    time_bandwidth = 4.0
    number_of_tapers = 7

    pasc = _load_mtdata('PASC.dat.gz')
    ado = _load_mtdata('ADO.dat.gz')

    pasc -= pasc.mean()
    ado -= ado.mean()

    npts = len(pasc)

    deconvolved, freq = mt_deconv(pasc, ado, sampling_rate,
                                  time_bandwidth=time_bandwidth,
                                  number_of_tapers=number_of_tapers,
                                  nfft=npts,
                                  demean=1, iadapt=0)


    Pdeconv = deconvolved[-500:][::-1]
    Pdeconv /= Pdeconv.max()

    nfft = 2*npts
    pasc = scipy.fftpack.fft(pasc, n=nfft)
    ado = scipy.fftpack.fft(ado, n=nfft )
    cc = pasc * ado.conj()

    cc = scipy.fftpack.ifft(cc)
    Pcc = cc[-500:][::-1]
    Pcc /= Pcc.max()


    time_bandwidth  = 1.5
    number_of_tapers = 1
    Dspec, Dfreq = mtspec(Pdeconv, sampling_rate,
                          time_bandwidth=time_bandwidth,
                          number_of_tapers=number_of_tapers)
    Cspec, Cfreq = mtspec(Pcc, sampling_rate,
                          time_bandwidth=time_bandwidth,
                          number_of_tapers=number_of_tapers)


    # Plotting
    plt.plot(np.arange(0,ngf), Pdeconv+3)
    plt.annotate("deconvolution", (200,3.5))
    plt.plot(np.arange(0,ngf), Pcc )
    plt.annotate("cross-correlation", (200,-0.5))

    plt.ylim(-1, 4.5)
    plt.yticks([],[])
    plt.xlabel("Time (s)")
    plt.ylabel("Amplitude")


    inset=plt.axes([0.7,0.35,0.18,0.25])
    plt.loglog(Dfreq, Dspec*1e5)
    plt.loglog(Cfreq, Cspec)
    plt.ylabel("log(PSD")
    plt.xlabel("frequency")
    plt.yticks([],[])
    plt.setp(inset,xticks=[])


    plt.show()

::

    import matplotlib as mpl
    mpl.rcParams['font.size'] = 9.0
    import matplotlib.pyplot as plt
    import numpy as np
    import scipy.fftpack

    from mtspec import mt_deconv, mtspec
    from mtspec.util import _load_mtdata

    sampling_rate = 1.0
    ngf = 500
    nf2 = ngf/2+1
    time_bandwidth = 4.0
    number_of_tapers = 7

    pasc = _load_mtdata('PASC.dat.gz')
    ado = _load_mtdata('ADO.dat.gz')

    pasc -= pasc.mean()
    ado -= ado.mean()

    npts = len(pasc)

    deconvolved, freq = mt_deconv(pasc, ado, sampling_rate,
                                  time_bandwidth=time_bandwidth,
                                  number_of_tapers=number_of_tapers,
                                  nfft=npts, demean=1, iadapt=0)


    Pdeconv = deconvolved[-500:][::-1]
    Pdeconv /= Pdeconv.max()

    nfft = 2*npts
    pasc = scipy.fftpack.fft(pasc, n=nfft)
    ado = scipy.fftpack.fft(ado, n=nfft )
    cc = pasc * ado.conj()

    cc = scipy.fftpack.ifft(cc)
    Pcc = cc[-500:][::-1]
    Pcc /= Pcc.max()


    time_bandwidth  = 1.5
    number_of_tapers = 1
    Dspec, Dfreq = mtspec(Pdeconv, sampling_rate,
                          time_bandwidth=time_bandwidth,
                          number_of_tapers=number_of_tapers)
    Cspec, Cfreq = mtspec(Pcc, sampling_rate,
                          time_bandwidth=time_bandwidth,
                          number_of_tapers=number_of_tapers)


    # Plotting
    plt.plot(np.arange(0,ngf), Pdeconv+3)
    plt.annotate("deconvolution", (200,3.5))
    plt.plot(np.arange(0,ngf), Pcc )
    plt.annotate("cross-correlation", (200,-0.5))

    plt.ylim(-1, 4.5)
    plt.yticks([],[])
    plt.xlabel("Time (s)")
    plt.ylabel("Amplitude")


    inset=plt.axes([0.7,0.35,0.18,0.25])
    plt.loglog(Dfreq, Dspec*1e5)
    plt.loglog(Cfreq, Cspec)
    plt.ylabel("log(PSD")
    plt.xlabel("frequency")
    plt.yticks([],[])
    plt.setp(inset,xticks=[])


    plt.show()
