import matplotlib.pylab as plt
from mtspec import mtspec, wigner_ville_spectrum
from mtspec.tests import chirp
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
