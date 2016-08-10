import matplotlib.pyplot as plt
import numpy as np
import scipy.fftpack

from mtspec import mt_deconv, mtspec
from mtspec.util import _load_mtdata

plt.style.use("ggplot")

sampling_rate = 1.0
ngf = 500
nf2 = ngf/2+1
time_bandwidth = 4.0
number_of_tapers = 7

pasc = _load_mtdata('PASC.dat.gz')
ado = _load_mtdata('ADO.dat.gz')

pasc -= pasc.mean()
ado -= ado.mean()

npts = len(pasc)

deconvolved, freq = mt_deconv(pasc, ado, sampling_rate,
                              time_bandwidth=time_bandwidth,
                              number_of_tapers=number_of_tapers,
                              nfft=npts, demean=1, iadapt=0)


Pdeconv = deconvolved[-500:][::-1]
Pdeconv /= Pdeconv.max()

nfft = 2 * npts
pasc = scipy.fftpack.fft(pasc, n=nfft)
ado = scipy.fftpack.fft(ado, n=nfft)
cc = pasc * ado.conj()

cc = scipy.fftpack.ifft(cc)
Pcc = cc[-500:][::-1]
Pcc /= Pcc.max()


time_bandwidth = 1.5
number_of_tapers = 1
Dspec, Dfreq = mtspec(Pdeconv, sampling_rate,
                      time_bandwidth=time_bandwidth,
                      number_of_tapers=number_of_tapers)
Cspec, Cfreq = mtspec(Pcc, sampling_rate,
                      time_bandwidth=time_bandwidth,
                      number_of_tapers=number_of_tapers)


# Plotting
plt.plot(np.arange(0, ngf), Pdeconv + 3)
plt.annotate("deconvolution", (200, 3.5))
plt.plot(np.arange(0, ngf), Pcc)
plt.annotate("cross-correlation", (200, -0.5))

plt.ylim(-1, 4.5)
plt.yticks([], [])
plt.xlabel("Time (s)")
plt.ylabel("Amplitude")


inset = plt.axes([0.7, 0.35, 0.18, 0.25])
plt.loglog(Dfreq, Dspec*1e5)
plt.loglog(Cfreq, Cspec)
plt.ylabel("log(PSD")
plt.xlabel("frequency")
plt.yticks([], [])
plt.setp(inset, xticks=[])


plt.show()
