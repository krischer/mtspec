import matplotlib as mpl
mpl.rcParams['font.size'] = 9.0
import matplotlib.pyplot as plt
plt.style.use("ggplot")
from mtspec import mtspec, sine_psd
from mtspec.util import _load_mtdata

data = _load_mtdata('PASC.dat.gz')

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
data = _load_mtdata('PASC.dat.gz')