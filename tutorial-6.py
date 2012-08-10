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