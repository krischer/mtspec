Tutorial
========

This tutorial gives source code and examples as well as figures how to
create the first three Figures of the multitaper paper of Prieto. More
Information can be found in the Prieto paper itself (reference_).

.. _reference: http://svn.geophysik.uni-muenchen.de/trac/mtspecpy/wiki


Recreate Fig. 1
---------------

.. plot::

    import matplotlib.pyplot as plt
    from mtspec import load_mtdata, mtspec

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

::

    import matplotlib.pyplot as plt
    from mtspec import load_mtdata, mtspec

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
    from mtspec import load_mtdata, mtspec

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
    from mtspec import load_mtdata, mtspec

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


Recreate Fig. 3
---------------

.. plot::

    import matplotlib.pyplot as plt
    from mtspec import load_mtdata, mtspec, sine_psd

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
    from mtspec import load_mtdata, mtspec, sine_psd

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
