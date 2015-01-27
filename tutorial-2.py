import matplotlib as mpl
mpl.rcParams['font.size'] = 9.0
import matplotlib.pyplot as plt
from mtspec import mtspec
from mtspec.util import _load_mtdata

data = _load_mtdata('v22_174_series.dat.gz')
spec, freq, jackknife, fstatistics, _ = mtspec(data, 4930., 3.5,
        number_of_tapers=5, nfft=312, statistics=True, rshape=0,
        fcrit=0.9)

fig = plt.figure()
ax1 = fig.add_subplot(2, 1, 1)
ax1.plot(freq, fstatistics, color='black')
ax1.set_xlim(freq[0], freq[-1])

ax2 = fig.add_subplot(2, 1, 2)
ax2.set_yscale('log')
ax2.plot(freq, spec, color='black')
ax2.set_xlim(freq[0], freq[-1])