import matplotlib as mpl
mpl.rcParams['font.size'] = 9.0
import matplotlib.pyplot as plt
from mtspec import mt_coherence
import numpy as np

# generate random series with 1Hz sinus inside
np.random.seed(815)
npts = 256
sampling_rate = 10.0
# one sine wave in one second (sampling_rate samples)
one_hz_sin = np.sin(np.arange(0, sampling_rate) /\
                    sampling_rate * 2 * np.pi)
one_hz_sin = np.tile(one_hz_sin, npts//sampling_rate + 1)[:npts]
xi = np.random.randn(npts) + one_hz_sin
xj = np.random.randn(npts) + one_hz_sin
dt, tbp, kspec, nf, p = 1.0/sampling_rate, 3.5, 5, npts/2, .90
# calculate mt_coherence
out = mt_coherence(dt, xi, xj, tbp, kspec, nf, p, freq=True,
                       cohe=True, iadapt=1)
# the plotting part
plt.subplot(211)
plt.plot(np.arange(npts)/sampling_rate, xi)
plt.plot(np.arange(npts)/sampling_rate, xj)
plt.subplot(212)
plt.plot(out['freq'], out['cohe'])
plt.show()