import matplotlib.pyplot as plt
plt.style.use("ggplot")

from mtspec import mtspec
from mtspec.util import _load_mtdata

data = _load_mtdata('v22_174_series.dat.gz')
spec, freq, jackknife, fstatistics, _ = mtspec(
    data, 4930., 3.5, number_of_tapers=5, nfft=312,
    statistics=True, rshape=0, fcrit=0.9)

# Convert to million years.
freq *= 1E6

fig = plt.figure()
ax1 = fig.add_subplot(2, 1, 1)
ax1.plot(freq, fstatistics, color='black')
ax1.set_xlim(freq[0], freq[-1])
ax1.set_xlabel("Frequency [c Ma$^{-1}]$")
ax1.set_ylabel("F-statistics for periodic lines")

ax2 = fig.add_subplot(2, 1, 2)
ax2.set_yscale('log')
ax2.plot(freq, spec, color='black')
ax2.set_xlim(freq[0], freq[-1])
ax2.set_xlabel("Frequency [c Ma$^{-1}]$")
ax2.set_ylabel("Power Spectral Density ($\delta^{18}O/ca^{-1}$)")

plt.tight_layout()
plt.show()
