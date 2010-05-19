# -*- coding: utf-8 -*-
"""
This test case will create some figure in Prieto et al. (2008) for a visual
comparision to see whether or not the wrapper works.

Currently produces figure 3.

References:
    * Prieto G, Parker R, Vernon III F. A Fortran 90 library for multitaper
      spectrum analysis. Computers & Geosciences. 2009;35(8):1701-1710.
"""

import matplotlib.pylab as plt
from mtspec import mtspec, sine_psd, mtspec_pad
import numpy as np
import os

# Output path.
outpath = os.path.join(os.path.dirname(__file__), 'output')
if not os.path.exists(outpath):
    os.mkdir(outpath)

####
# Figure 1.
####
datafile = os.path.join(os.path.dirname(__file__), 'data', 'v22_174_series.dat')
data = np.loadtxt(datafile)
length = len(data)

fig = plt.figure()
ax1 = fig.add_subplot(2, 1, 1)
ax1.plot(data, color='black')
ax1.set_xlim(0, length)

ax2 = fig.add_subplot(2, 1, 2)
#spec, freq, jackknife, _, _  = mtspec_pad(data, 312, 4930., 3.5, number_of_tapers=5,
#                                          statistics = True)
spec, freq, jackknife, _, _ = mtspec_pad(data, 312, 4930., 3.5, number_of_tapers=5, 
                         statistics=True)
ax2.set_yscale('log')
ax2.plot(freq, spec, color='black')
try:
    ax2.fill_between(freq, jackknife[:, 0], jackknife[:, 1], color='grey')
except:
    ax2.plot(freq, jackknife[:, 0], '--', color = 'red')
    ax2.plot(freq, jackknife[:, 1], '--', color = 'red')
ax2.set_xlim(freq[0], freq[-1])

outfile = os.path.join(outpath, 'fig1.pdf')
fig.savefig(outfile)

####
# Figure 3.
####
datafile = os.path.join(os.path.dirname(__file__), 'data', 'PASC.dat')
data = np.loadtxt(datafile)
length = len(data)

fig = plt.figure()
ax1 = fig.add_subplot(3, 1, 1)
ax1.plot(data, color='black')
ax1.set_xlim(0, length)

ax2 = fig.add_subplot(3, 2, 3)
spec, freq = mtspec(data, 1.0, 1.5, number_of_tapers=1)
ax2.set_yscale('log')
ax2.set_xscale('log')
ax2.plot(freq, spec, color='black')
ax2.set_xlim(freq[0], freq[-1])

ax3 = fig.add_subplot(3, 2, 4)
spec, freq = mtspec(data, 1.0, 4.5, number_of_tapers=5)
ax3.set_yscale('log')
ax3.set_xscale('log')
ax3.plot(freq, spec, color='black')
ax3.set_xlim(freq[0], freq[-1])

ax4 = fig.add_subplot(3, 2, 5)
spec, freq = sine_psd(data, 1.0)
ax4.set_yscale('log')
ax4.set_xscale('log')
ax4.plot(freq, spec, color='black')
ax4.set_xlim(freq[0], freq[-1])

ax5 = fig.add_subplot(3, 2, 6)
spec, freq = mtspec(data, 1.0, 4.5, number_of_tapers=5, quadratic=True)
ax5.set_yscale('log')
ax5.set_xscale('log')
ax5.plot(freq, spec, color='black')
ax5.set_xlim(freq[0], freq[-1])

outfile = os.path.join(outpath, 'fig3.pdf')
fig.savefig(outfile)
