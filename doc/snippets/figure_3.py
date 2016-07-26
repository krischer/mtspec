import matplotlib.pyplot as plt
plt.style.use("ggplot")

from mtspec import mtspec, sine_psd
from mtspec.util import _load_mtdata

data = _load_mtdata('PASC.dat.gz')

plt.subplot(311)
plt.plot(data, color='black')
plt.xlim(0, len(data))

spec, freq = mtspec(data, 1.0, 1.5, number_of_tapers=1)

plt.subplot(323)
plt.loglog(freq, spec, color='black')
plt.xlim(freq[0], freq[-1])
plt.text(x=0.5, y=0.85, s="Single Taper",
         transform=plt.gca().transAxes, ha="center")

spec, freq = mtspec(data, 1.0, 4.5, number_of_tapers=5)

plt.subplot(324)
plt.loglog(freq, spec, color='black')
plt.xlim(freq[0], freq[-1])
plt.text(x=0.5, y=0.85, s="5 Tapers Multitaper",
         transform=plt.gca().transAxes, ha="center")

spec, freq = sine_psd(data, 1.0)

plt.subplot(325)
plt.loglog(freq, spec, color='black')
plt.xlim(freq[0], freq[-1])
plt.text(x=0.5, y=0.85, s="Sine Multitaper",
         transform=plt.gca().transAxes, ha="center")

spec, freq = mtspec(data, 1.0, 4.5, number_of_tapers=5,
                    quadratic=True)

plt.subplot(326)
plt.loglog(freq, spec, color='black')
plt.xlim(freq[0], freq[-1])
plt.text(x=0.5, y=0.85, s="Quadratic Multitaper",
         transform=plt.gca().transAxes, ha="center")

plt.tight_layout()
plt.show()
