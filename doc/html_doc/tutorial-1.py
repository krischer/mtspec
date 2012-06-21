import matplotlib as mpl
mpl.rcParams['font.size'] = 9.0
import matplotlib.pyplot as plt
from mtspec import mtspec
from mtspec.util import load_mtdata

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