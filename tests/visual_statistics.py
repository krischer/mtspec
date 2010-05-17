# -*- coding: utf-8 -*-

from obspy.core import read
import matplotlib.pyplot as plt
from multitaper import mtspec, sine_psd
import numpy as np

st = read('test_file.gse2')
st[0].data = np.require(st[0].data, 'float32')

# Level and demean data.
st[0].data -= st[0].data.mean()
st[0].data -= np.linspace(st[0].data[0], st[0].data[-1],
                          len(st[0].data))

# Calculate multitaper spectrums.
spec, freq, jackknife, _, _ = mtspec(st[0].data, st[0].stats.delta, 4, statistics = True)
sinespec, sinefreq, sine_errors, _ = sine_psd(st[0].data, st[0].stats.delta,
                                              statistics = True)
print jackknife

plt.subplot(311)
plt.title('Data (%.2f Hz)' % st[0].stats.sampling_rate)
plt.plot(st[0].data)
plt.xlim(0, len(st[0].data))

plt.subplot(312)
plt.fill_between(freq, spec + jackknife[:, 1]/2.0, spec - jackknife[:, 1]/2.0,
                 color = 'grey')
plt.plot(freq, spec)
plt.title('Adaptive Multitaper Spectrum')
plt.yscale('log')
plt.xlim(freq[0], freq[-1])

plt.subplot(313)
plt.fill_between(sinefreq, sine_errors[:, 0], sine_errors[:, 1], color = 'grey')
plt.plot(sinefreq, sinespec)
plt.plot(sinefreq, sine_errors[:, 0], '--', color = 'red')
plt.plot(sinefreq, sine_errors[:, 1], '--', color = 'red')
plt.title('Sine Multitaper Spectrum')
plt.yscale('log')
plt.xlim(sinefreq[0], sinefreq[-1])

plt.show()
