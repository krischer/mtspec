# -*- coding: utf-8 -*-

from obspy.core import read
import matplotlib.pyplot as plt
from mtspec import mtspec, sine_psd
import numpy as np
import os

st = read(os.path.join('data','test_file.gse2'))
st[0].data = np.require(st[0].data, 'float32')

# Level and demean data.
st[0].data -= st[0].data.mean()
st[0].data -= np.linspace(st[0].data[0], st[0].data[-1],
                          len(st[0].data))

# Calculate multitaper spectrums.
spec, freq = mtspec(st[0].data, st[0].stats.delta, 4)
quadspec, quadfreq = mtspec(st[0].data, st[0].stats.delta, 4,
                      quadratic = True)
sinespec, sinefreq = sine_psd(st[0].data, st[0].stats.delta)

plt.subplot(321)
plt.title('Data (%.2f Hz)' % st[0].stats.sampling_rate)
plt.plot(st[0].data)
plt.xlim(0, len(st[0].data))

plt.subplot(322)
fft = np.fft.rfft(st[0].data)**2
fftfreqs = np.linspace(0, 0.5*st[0].stats.sampling_rate, len(fft))
plt.plot(fftfreqs, fft)
plt.title('Boxcar tapered spectrum')
plt.yscale('log')
plt.xlim(fftfreqs[0], fftfreqs[-1])

plt.subplot(323)
plt.plot(fftfreqs, np.fft.rfft(np.hanning(len(st[0].data)) * st[0].data)**2)
plt.title('Hanning tapered spectrum')
plt.yscale('log')
plt.xlim(fftfreqs[0], fftfreqs[-1])

plt.subplot(324)
plt.plot(freq, spec)
plt.title('Adaptive Multitaper Spectrum')
plt.yscale('log')
plt.xlim(freq[0], freq[-1])

plt.subplot(325)
plt.plot(quadfreq, quadspec)
plt.title('Adaptive Quadratic Multitaper Spectrum')
plt.yscale('log')
plt.xlim(quadfreq[0], quadfreq[-1])

plt.subplot(326)
plt.plot(sinefreq, sinespec)
plt.title('Sine Multitaper Spectrum (2 iterations)')
plt.yscale('log')
plt.xlim(sinefreq[0], sinefreq[-1])

plt.show()
