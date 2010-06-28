import matplotlib.pylab as plt
from mtspec import mtspec, wigner_ville_spectrum
import numpy as np
import sys

length = 5 * 512

# Baseline low frequency plut noise.
data = np.sin(np.linspace(0, 80 * np.pi, length))
noise = np.random.ranf(length)
noise /= noise.max()
noise /= 15
data += noise

# Double last two fifths of the signal.
data[-2 * 512:] *= 2.0
blub1 = 2.5 * np.sin(np.linspace(0, 400 * np.pi, 512))
blub1 *= np.linspace(1, 0, 512)
data[512:2 * 512] += blub1
# Add second transient signal.
blub2 = 5.0 * np.sin(np.linspace(0, 200 * np.pi, 512))
blub2 *= np.linspace(1, 0, 512)
data[3 * 512:4 * 512] += blub2

fig = plt.figure()

# Plot the data
ax1 = fig.add_axes([0.2,0.75, 0.79, 0.24])
ax1.plot(data)
ax1.set_xlim(0, length)

# Plot the spectrum.
ax2 = fig.add_axes([0.01,0.01,0.18,0.69])
spec, freq = mtspec(data, 10, 3.5)
ax2.plot(spec[::-1], freq)
ax2.set_xlim(0, spec.max())
ax2.set_ylim(freq[0], freq[-1])
ax2.set_yticks([])
ax2.set_xticks([])

# Create the wigner ville spectrum.
wv = wigner_ville_spectrum(data, 10, 3.5, smoothing_filter='gauss', verbose=True)
wv = abs(wv)
wv[wv < 0.0] = 0.0

# Plot the WV.
ax3 = fig.add_axes([0.2, 0.01, 0.79, 0.69])
ax3.set_yticks([])
ax3.set_xticks([])
ax3.imshow(wv, interpolation='nearest', aspect='auto')

plt.show()
