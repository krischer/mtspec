import numpy as np
import matplotlib.pyplot as plt
import obspy
from obspy.signal.filter import lowpass

from mtspec import mt_deconvolve, mtspec

plt.style.use('ggplot')

Main = obspy.read('data/main')
EGF = obspy.read('data/egf')

station_name = Main[0].stats.station
fr = Main[0].stats.sampling_rate
# seconds
dt = 1 / fr

r = mt_deconvolve(Main[0].data, EGF[0].data, dt, nfft=len(Main[0].data),
                  time_bandwidth=4, number_of_tapers=7,
                  weights='constant', demean=True)

decon = r['deconvolved']
freq = r['frequencies']

# Time vector for RSTF.
time = np.arange(0, len(decon))*dt

M = np.arange(0, len(decon))
N = len(M)

SeD = np.where(np.logical_and(M >= 0, M <= N / 2))
d1 = decon[SeD]

SeD2 = np.where(np.logical_and(M > N / 2, M <= N + 1))
d2 = decon[SeD2]

# Relative source time function
stf = np.concatenate((d2, d1))

# Cleaning the rSTF from high frequency noise
stf = lowpass(stf, 4, fr, corners=4, zerophase=True)
stf /= stf.max()

# Fourier Transform of the rSTF
Cspec, Cfreq = mtspec(stf, delta=dt, time_bandwidth=2,
                      number_of_tapers=3)

m = len(Cspec)
Cspec = Cspec[:m // 2]
Cfreq = Cfreq[:m // 2]

# Creating figure
fig = plt.figure()
ax1 = fig.add_subplot(211)
ax1.loglog(Cfreq, Cspec, 'grey', linewidth=1.7, label='Spectral ratio')
ax1.set_xlabel("Frequency [Hz]")
ax1.set_ylabel("Amplitude")
plt.grid(True, which="both", ls="-", color='grey')
plt.legend()

ax2 = fig.add_subplot(212)
ax2.plot(time, stf, 'k', linewidth=1.5, label='Source Time function')
ax2.fill_between(time, stf, facecolor='green', alpha=0.3)
ax2.set_xlabel("Time [s]")
ax2.set_ylim(-0.5, 1.5)

plt.legend()
plt.suptitle('Station: ' + station_name, fontsize=16)

plt.show()
