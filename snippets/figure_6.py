import matplotlib.pyplot as plt
import numpy as np
import scipy.fftpack

from mtspec import mt_deconvolve, mtspec
from mtspec.util import _load_mtdata

plt.style.use("ggplot")

# Load and demean data.
pasc = _load_mtdata('PASC.dat.gz')
ado = _load_mtdata('ADO.dat.gz')
pasc -= pasc.mean()
ado -= ado.mean()

r = mt_deconvolve(pasc, ado, delta=1.0,
                  time_bandwidth=4.0,
                  number_of_tapers=7,
                  nfft=len(pasc), demean=True,
                  weights="adaptive")

deconvolved = r["deconvolved"]

Pdeconv = deconvolved[-500:][::-1]
Pdeconv /= Pdeconv.max()

nfft = 2 * len(pasc)
pasc = scipy.fftpack.fft(pasc, n=nfft)
ado = scipy.fftpack.fft(ado, n=nfft)
cc = pasc * ado.conj()

cc = scipy.fftpack.ifft(cc).real
Pcc = cc[-500:][::-1]
Pcc /= Pcc.max()


Dspec, Dfreq = mtspec(Pdeconv, delta=1.0,
                      time_bandwidth=1.5,
                      number_of_tapers=1)
Cspec, Cfreq = mtspec(Pcc, delta=1.0,
                      time_bandwidth=1.5,
                      number_of_tapers=1)

# Plotting
plt.plot(np.arange(0, 500), Pdeconv + 3)
plt.annotate("deconvolution", (200, 3.5))
plt.plot(np.arange(0, 500), Pcc)
plt.annotate("cross-correlation", (200, -0.5))

plt.ylim(-1, 4.5)
plt.yticks([], [])
plt.xlabel("Time (s)")
plt.ylabel("Amplitude")

inset = plt.axes([0.7, 0.35, 0.18, 0.25])
plt.loglog(Dfreq, Dspec*1e5)
plt.loglog(Cfreq, Cspec)
plt.ylabel("log(PSD)")
plt.xlabel("frequency")
plt.yticks([], [])
plt.setp(inset, xticks=[])

plt.show()
