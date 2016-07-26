import matplotlib.pyplot as plt
plt.style.use("ggplot")

import numpy as np

from mtspec import mtspec
from mtspec.util import _load_mtdata

data = _load_mtdata("v22_174_series.dat.gz")
spec, freq, jackknife, fstatistics, _ = mtspec(
    data=data, delta=4930., time_bandwidth=3.5, number_of_tapers=5,
    nfft=312, statistics=True, rshape=0, fcrit=0.9)

# Convert to million years.
freq *= 1E6

plt.subplot(211)
plt.plot(freq, fstatistics, color="black")
plt.xlim(freq[0], freq[-1])
plt.xlabel("Frequency [c Ma$^{-1}]$")
plt.ylabel("F-statistics for periodic lines")

# Plot the confidence intervals.
for p in [90, 95, 99]:
    y = np.percentile(fstatistics, p)
    plt.hlines(y, freq[0], freq[-1], linestyles="--", color="0.2")
    plt.text(x=99, y=y + 0.2, s="%i %%" % p, ha="right")

plt.subplot(212)
plt.semilogy(freq, spec, color="black")
plt.xlim(freq[0], freq[-1])
plt.xlabel("Frequency [c Ma$^{-1}]$")
plt.ylabel("Power Spectral Density ($\delta^{18}O/ca^{-1}$)")

plt.tight_layout()
plt.show()
