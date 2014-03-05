import matplotlib as mpl
mpl.rcParams['font.size'] = 9.0
import matplotlib.pyplot as plt
from mtspec import dpss

tapers, lamb, theta = dpss(512, 2.5, 10)

ax = plt.figure().add_subplot(111)
for i in range(10):
    ax.plot(tapers[:,i])
ax.set_xlim(0, len(tapers[:,0]))