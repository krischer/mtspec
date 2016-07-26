import matplotlib.pyplot as plt
plt.style.use("ggplot")

from mtspec import dpss

tapers, lamb, theta = dpss(512, 2.5, 10)

ax = plt.figure().add_subplot(111)
for i in range(10):
    ax.plot(tapers[:,i])
ax.set_xlim(0, len(tapers[:,0]))

plt.show()
