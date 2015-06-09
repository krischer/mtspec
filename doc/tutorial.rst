Tutorial
========

This tutorial gives source code and examples as well as figures how to
create the first three Figures of the multitaper paper of Prieto.
A simple example for the dpss method and the Wigner Ville spectrum is
included as well. More information can be found in the Prieto paper itself
(reference_).

.. _reference: https://github.com/krischer/mtspec


Recreate Fig. 1
---------------
.. plot::

    import matplotlib as mpl
    mpl.rcParams['font.size'] = 9.0
    import matplotlib.pyplot as plt
    from mtspec import mtspec
    from mtspec.util import _load_mtdata

    data = _load_mtdata('v22_174_series.dat.gz')

    spec, freq, jackknife, _, _ = mtspec(data, 4930., 3.5, number_of_tapers=5,
                                         nfft=312, statistics=True)

    fig = plt.figure()
    ax1 = fig.add_subplot(2, 1, 1)
    ax1.plot(data, color='black')
    ax1.set_xlim(0, len(data))

    ax2 = fig.add_subplot(2, 1, 2)
    ax2.set_yscale('log')
    ax2.plot(freq, spec, color='black')
    ax2.plot(freq, jackknife[:, 0], '--', color = 'red')
    ax2.plot(freq, jackknife[:, 1], '--', color = 'red')
    ax2.set_xlim(freq[0], freq[-1])

::

    import matplotlib.pyplot as plt
    from mtspec import mtspec
    from mtspec.util import _load_mtdata

    data = _load_mtdata('v22_174_series.dat.gz')

    spec, freq, jackknife, _, _ = mtspec(data, 4930., 3.5, number_of_tapers=5,
                                         nfft=312, statistics=True)

    fig = plt.figure()
    ax1 = fig.add_subplot(2, 1, 1)
    ax1.plot(data, color='black')
    ax1.set_xlim(0, len(data))

    ax2 = fig.add_subplot(2, 1, 2)
    ax2.set_yscale('log')
    ax2.plot(freq, spec, color='black')
    ax2.plot(freq, jackknife[:, 0], '--', color = 'red')
    ax2.plot(freq, jackknife[:, 1], '--', color = 'red')
    ax2.set_xlim(freq[0], freq[-1])
    plt.show()


Recreate Fig. 2
---------------
.. plot::

    import matplotlib as mpl
    mpl.rcParams['font.size'] = 9.0
    import matplotlib.pyplot as plt
    from mtspec import mtspec
    from mtspec.util import _load_mtdata

    data = _load_mtdata('v22_174_series.dat.gz')
    spec, freq, jackknife, fstatistics, _ = mtspec(data, 4930., 3.5,
            number_of_tapers=5, nfft=312, statistics=True, rshape=0,
            fcrit=0.9)

    fig = plt.figure()
    ax1 = fig.add_subplot(2, 1, 1)
    ax1.plot(freq, fstatistics, color='black')
    ax1.set_xlim(freq[0], freq[-1])

    ax2 = fig.add_subplot(2, 1, 2)
    ax2.set_yscale('log')
    ax2.plot(freq, spec, color='black')
    ax2.set_xlim(freq[0], freq[-1])

::

    import matplotlib.pyplot as plt
    from mtspec import mtspec
    from mtspec.util import _load_mtdata

    data = _load_mtdata('v22_174_series.dat.gz')
    spec, freq, jackknife, fstatistics, _ = mtspec(data, 4930., 3.5,
            number_of_tapers=5, nfft=312, statistics=True, rshape=0,
            fcrit=0.9)

    fig = plt.figure()
    ax1 = fig.add_subplot(2, 1, 1)
    ax1.plot(freq, fstatistics, color='black')
    ax1.set_xlim(freq[0], freq[-1])

    ax2 = fig.add_subplot(2, 1, 2)
    ax2.set_yscale('log')
    ax2.plot(freq, spec, color='black')
    ax2.set_xlim(freq[0], freq[-1])
    plt.show()


Recreate Fig. 3
---------------
.. plot::

    import matplotlib as mpl
    mpl.rcParams['font.size'] = 9.0
    import matplotlib.pyplot as plt
    from mtspec import mtspec, sine_psd
    from mtspec.util import _load_mtdata

    data = _load_mtdata('PASC.dat.gz')

    fig = plt.figure()
    ax1 = fig.add_subplot(3, 1, 1)
    ax1.plot(data, color='black')
    ax1.set_xlim(0, len(data))

    spec, freq = mtspec(data, 1.0, 1.5, number_of_tapers=1)

    ax2 = fig.add_subplot(3, 2, 3)
    ax2.set_yscale('log')
    ax2.set_xscale('log')
    ax2.plot(freq, spec, color='black')
    ax2.set_xlim(freq[0], freq[-1])

    spec, freq = mtspec(data, 1.0, 4.5, number_of_tapers=5)

    ax3 = fig.add_subplot(3, 2, 4)
    ax3.set_yscale('log')
    ax3.set_xscale('log')
    ax3.plot(freq, spec, color='black')
    ax3.set_xlim(freq[0], freq[-1])

    spec, freq = sine_psd(data, 1.0)

    ax4 = fig.add_subplot(3, 2, 5)
    ax4.set_yscale('log')
    ax4.set_xscale('log')
    ax4.plot(freq, spec, color='black')
    ax4.set_xlim(freq[0], freq[-1])

    spec, freq = mtspec(data, 1.0, 4.5, number_of_tapers=5, quadratic=True)

    ax5 = fig.add_subplot(3, 2, 6)
    ax5.set_yscale('log')
    ax5.set_xscale('log')
    ax5.plot(freq, spec, color='black')
    ax5.set_xlim(freq[0], freq[-1])
    data = _load_mtdata('PASC.dat.gz')

::

    import matplotlib.pyplot as plt
    from mtspec import mtspec, sine_psd
    from mtspec.util import _load_mtdata

    fig = plt.figure()
    ax1 = fig.add_subplot(3, 1, 1)
    ax1.plot(data, color='black')
    ax1.set_xlim(0, len(data))

    spec, freq = mtspec(data, 1.0, 1.5, number_of_tapers=1)

    ax2 = fig.add_subplot(3, 2, 3)
    ax2.set_yscale('log')
    ax2.set_xscale('log')
    ax2.plot(freq, spec, color='black')
    ax2.set_xlim(freq[0], freq[-1])

    spec, freq = mtspec(data, 1.0, 4.5, number_of_tapers=5)

    ax3 = fig.add_subplot(3, 2, 4)
    ax3.set_yscale('log')
    ax3.set_xscale('log')
    ax3.plot(freq, spec, color='black')
    ax3.set_xlim(freq[0], freq[-1])

    spec, freq = sine_psd(data, 1.0)

    ax4 = fig.add_subplot(3, 2, 5)
    ax4.set_yscale('log')
    ax4.set_xscale('log')
    ax4.plot(freq, spec, color='black')
    ax4.set_xlim(freq[0], freq[-1])

    spec, freq = mtspec(data, 1.0, 4.5, number_of_tapers=5, quadratic=True)

    ax5 = fig.add_subplot(3, 2, 6)
    ax5.set_yscale('log')
    ax5.set_xscale('log')
    ax5.plot(freq, spec, color='black')
    ax5.set_xlim(freq[0], freq[-1])
    plt.show()


dpss Example
------------
.. plot::

    import matplotlib as mpl
    mpl.rcParams['font.size'] = 9.0
    import matplotlib.pyplot as plt
    from mtspec import dpss

    tapers, lamb, theta = dpss(512, 2.5, 10)

    ax = plt.figure().add_subplot(111)
    for i in xrange(10):
        ax.plot(tapers[:,i])
    ax.set_xlim(0, len(tapers[:,0]))

::

    import matplotlib.pyplot as plt
    from mtspec import dpss

    tapers, lamb, theta = dpss(512, 2.5, 10)

    ax = plt.figure().add_subplot(111)
    for i in xrange(10):
        ax.plot(tapers[:,i])
    ax.set_xlim(0, len(tapers[:,0]))
    plt.show()


Wigner-Ville Spectrum
---------------------
.. plot::

    import matplotlib as mpl
    mpl.rcParams['font.size'] = 9.0
    import matplotlib.pyplot as plt
    from mtspec import mtspec, wigner_ville_spectrum
    from mtspec.util import signal_bursts
    import numpy as np

    fig = plt.figure()
    data = signal_bursts()

    # Plot the data
    ax1 = fig.add_axes([0.2,0.75, 0.79, 0.24])
    ax1.plot(data)
    ax1.set_xlim(0, len(data))

    # Plot multitaper spectrum
    ax2 = fig.add_axes([0.06,0.02,0.13,0.69])
    spec, freq = mtspec(data, 10, 3.5)
    ax2.plot(spec, freq)
    ax2.set_xlim(0, spec.max())
    ax2.set_ylim(freq[0], freq[-1])
    ax2.set_xticks([])

    # Create the wigner ville spectrum
    wv = wigner_ville_spectrum(data, 10, 3.5, smoothing_filter='gauss')

    # Plot the WV
    ax3 = fig.add_axes([0.2, 0.02, 0.79, 0.69])
    ax3.set_yticks([])
    ax3.set_xticks([])
    ax3.imshow(abs(wv), interpolation='nearest', aspect='auto')
    plt.show()

::

    import matplotlib.pyplot as plt
    from mtspec import mtspec, wigner_ville_spectrum
    from mtspec.util import signal_bursts
    import numpy as np

    fig = plt.figure()
    data = signal_bursts()

    # Plot the data
    ax1 = fig.add_axes([0.2,0.75, 0.79, 0.24])
    ax1.plot(data)
    ax1.set_xlim(0, len(data))

    # Plot multitaper spectrum
    ax2 = fig.add_axes([0.06,0.02,0.13,0.69])
    spec, freq = mtspec(data, 10, 3.5)
    ax2.plot(spec, freq)
    ax2.set_xlim(0, spec.max())
    ax2.set_ylim(freq[0], freq[-1])
    ax2.set_xticks([])

    # Create the wigner ville spectrum
    wv = wigner_ville_spectrum(data, 10, 3.5, smoothing_filter='gauss')

    # Plot the WV
    ax3 = fig.add_axes([0.2, 0.02, 0.79, 0.69])
    ax3.set_yticks([])
    ax3.set_xticks([])
    ax3.imshow(abs(wv), interpolation='nearest', aspect='auto')
    plt.show()

Wigner-Ville Smoothing
++++++++++++++++++++++
One of the main disadvantages of the Wigner-Ville method is occurrence of interference terms. This can be somewhat alleviated by smoothing it at the cost of the clarity of the distribution.

The following plot shows an example of this behaviour. The signal consists of a linear chirp and an exponential chirp. The left figure is with smoothing and the right one without it.

.. plot::

    import matplotlib as mpl
    mpl.rcParams['font.size'] = 9.0
    import matplotlib.pylab as plt
    from mtspec import wigner_ville_spectrum
    from mtspec.util import linear_chirp, exponential_chirp
    import numpy as np

    fig = plt.figure()
    data = linear_chirp() + exponential_chirp()

    # Plot the data
    ax1 = fig.add_axes([0.05,0.75, 0.90, 0.24])
    ax1.plot(data)
    ax1.set_xlim(0, len(data))
    ax1.set_yticks([])

    # Get the smoothed WV spectrum.
    wv = wigner_ville_spectrum(data, 10, 5.0, smoothing_filter='gauss',
                               filter_width=25)

    # Plot the WV
    ax2 = fig.add_axes([0.01, 0.025, 0.48, 0.64])
    ax2.set_yticks([])
    ax2.set_xticks([])
    ax2.imshow(abs(wv), interpolation='nearest', aspect='auto')
    ax2.set_title('With smoothing')

    # Get the WV spectrum.
    wv = wigner_ville_spectrum(data, 10, 5.0, smoothing_filter=None)

    # Plot the WV
    ax3 = fig.add_axes([0.51, 0.025, 0.48, 0.64])
    ax3.set_yticks([])
    ax3.set_xticks([])
    ax3.imshow(abs(wv), interpolation='nearest', aspect='auto')
    ax3.set_title('Without smoothing')

    plt.show()
[`source code <https://github.com/krischer/mtspec/blob/master/doc/wv_smoothing.py>`_]


Multitaper coherence example
----------------------------
.. plot::

    import matplotlib as mpl
    mpl.rcParams['font.size'] = 9.0
    import matplotlib.pyplot as plt
    from mtspec import mt_coherence
    import numpy as np

    # generate random series with 1Hz sinus inside
    np.random.seed(815)
    npts = 256
    sampling_rate = 10.0
    # one sine wave in one second (sampling_rate samples)
    one_hz_sin = np.sin(np.arange(0, sampling_rate) /\
                        sampling_rate * 2 * np.pi)
    one_hz_sin = np.tile(one_hz_sin, npts//sampling_rate + 1)[:npts]
    xi = np.random.randn(npts) + one_hz_sin
    xj = np.random.randn(npts) + one_hz_sin
    dt, tbp, kspec, nf, p = 1.0/sampling_rate, 3.5, 5, npts/2, .90
    # calculate mt_coherence
    out = mt_coherence(dt, xi, xj, tbp, kspec, nf, p, freq=True,
                           cohe=True, iadapt=1)
    # the plotting part
    plt.subplot(211)
    plt.plot(np.arange(npts)/sampling_rate, xi)
    plt.plot(np.arange(npts)/sampling_rate, xj)
    plt.subplot(212)
    plt.plot(out['freq'], out['cohe'])
    plt.show()

::

    import matplotlib.pyplot as plt
    from mtspec import mt_coherence
    import numpy as np

    # generate random series with 1Hz sinus inside
    np.random.seed(815)
    npts = 256
    sampling_rate = 10.0
    # one sine wave in one second (sampling_rate samples)
    one_hz_sin = np.sin(np.arange(0, sampling_rate) /\
                        sampling_rate * 2 * np.pi)
    one_hz_sin = np.tile(one_hz_sin, npts//sampling_rate + 1)[:npts]
    xi = np.random.randn(npts) + one_hz_sin
    xj = np.random.randn(npts) + one_hz_sin
    dt, tbp, kspec, nf, p = 1.0/sampling_rate, 3.5, 5, npts/2, .90
    # calculate mt_coherence
    out = mt_coherence(dt, xi, xj, tbp, kspec, nf, p, freq=True,
                           cohe=True, iadapt=1)
    # the plotting part
    plt.subplot(211)
    plt.plot(np.arange(npts)/sampling_rate, xi)
    plt.plot(np.arange(npts)/sampling_rate, xj)
    plt.subplot(212)
    plt.plot(out['freq'], out['cohe'])
    plt.show()

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
