Tutorial
========

This tutorial gives source code and examples as well as figures how to
create the first three Figures of the multitaper paper of Prieto. 
A simple example for the dpss method and the Wigner Ville spectrum is
included as well. More information can be found in the Prieto paper itself
(reference_).  

.. _reference: http://svn.geophysik.uni-muenchen.de/trac/mtspecpy/wiki


Recreate Fig. 1
---------------
.. plot::

    import matplotlib.pyplot as plt
    from mtspec import mtspec
    from mtspec.util import load_mtdata

    data = load_mtdata('v22_174_series.dat.gz')

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
    from mtspec.util import load_mtdata

    data = load_mtdata('v22_174_series.dat.gz')

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

    import matplotlib.pyplot as plt
    from mtspec import mtspec
    from mtspec.util import load_mtdata

    data = load_mtdata('v22_174_series.dat.gz')
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
    from mtspec.util import load_mtdata

    data = load_mtdata('v22_174_series.dat.gz')
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

    import matplotlib.pyplot as plt
    from mtspec import mtspec, sine_psd
    from mtspec.util import load_mtdata

    data = load_mtdata('PASC.dat.gz')

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
    data = load_mtdata('PASC.dat.gz')

::

    import matplotlib.pyplot as plt
    from mtspec import mtspec, sine_psd
    from mtspec.util import load_mtdata

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


Wigner Ville Spectrum Example
-----------------------------
.. plot::

    import matplotlib.pyplot as plt
    from mtspec import mtspec, wigner_ville_spectrum
    from mtspec.util import chirp
    import numpy as np

    fig = plt.figure()
    data = chirp()

    # Plot the data
    ax1 = fig.add_axes([0.2,0.75, 0.79, 0.24])
    ax1.plot(data)
    ax1.set_xlim(0, len(data))

    # Plot multitaper spectrum
    ax2 = fig.add_axes([0.01,0.01,0.18,0.69])
    spec, freq = mtspec(data, 10, 3.5)
    ax2.plot(spec[::-1], freq)
    ax2.set_xlim(0, spec.max())
    ax2.set_ylim(freq[0], freq[-1])
    ax2.set_yticks([])
    ax2.set_xticks([])

    # Create the wigner ville spectrum
    wv = wigner_ville_spectrum(data, 10, 3.5, smoothing_filter='gauss', frac=2)

    # Plot the WV
    ax3 = fig.add_axes([0.2, 0.01, 0.79, 0.69])
    ax3.set_yticks([])
    ax3.set_xticks([])
    ax3.imshow(abs(wv), interpolation='nearest', aspect='auto')

::

    import matplotlib.pyplot as plt
    from mtspec import mtspec, wigner_ville_spectrum
    from mtspec.util import chirp
    import numpy as np

    fig = plt.figure()
    data = chirp()

    # Plot the data
    ax1 = fig.add_axes([0.2,0.75, 0.79, 0.24])
    ax1.plot(data)
    ax1.set_xlim(0, len(data))

    # Plot multitaper spectrum
    ax2 = fig.add_axes([0.01,0.01,0.18,0.69])
    spec, freq = mtspec(data, 10, 3.5)
    ax2.plot(spec[::-1], freq)
    ax2.set_xlim(0, spec.max())
    ax2.set_ylim(freq[0], freq[-1])
    ax2.set_yticks([])
    ax2.set_xticks([])

    # Create the wigner ville spectrum
    wv = wigner_ville_spectrum(data, 10, 3.5, smoothing_filter='gauss', frac=2)

    # Plot the WV
    ax3 = fig.add_axes([0.2, 0.01, 0.79, 0.69])
    ax3.set_yticks([])
    ax3.set_xticks([])
    ax3.imshow(abs(wv), interpolation='nearest', aspect='auto')
    plt.show()
