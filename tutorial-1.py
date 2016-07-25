import matplotlib as mpl
mpl.rcParams['font.size'] = 9.0
import matplotlib.pyplot as plt
plt.style.use("ggplot")
from mtspec import mtspec
from mtspec.util import _load_mtdata

data = _load_mtdata('v22_174_series.dat.gz')

spec, freq, jackknife, _, _ = mtspec(
    data=data, delta=4930.0, time_bandwidth=3.5,
    number_of_tapers=5, nfft=312, statistics=True)

fig = plt.figure()
ax1 = fig.add_subplot(2, 1, 1)
# Plot thousands of years.
ax1.plot(np.arange(len(data)) * 4.930, data, color='black')
ax1.set_xlim(0, 800)
ax1.set_ylim(-1.0, 1.0)
ax1.set_xlabel("Time [1000 years]")
ax1.set_ylabel("Change in $\delta^{18}O$")

ax2 = fig.add_subplot(2, 1, 2)
ax2.set_yscale('log')
freq *= 1E6
ax2.plot(freq, spec, color='black')
ax2.fill_between(freq, jackknife[:, 0], jackknife[:, 1], color="red",
                 alpha=0.3)
ax2.set_xlim(freq[0], freq[-1])
ax2.set_ylim(0.1E1, 1E5)
ax2.set_xlabel("Frequency [c Ma$^{-1}]$")
ax2.set_ylabel("Power Spectral Density ($\delta^{18}O/ca^{-1}$)")

plt.tight_layout()